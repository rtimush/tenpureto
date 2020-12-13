{-# LANGUAGE TupleSections #-}

module Tenpureto.GraphTest where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           Algebra.Graph.ToGraph
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
import           Data.Ix
import           Data.List
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )

import           Tenpureto.Graph

test_filterVertices :: [TestTree]
test_filterVertices =
    [ testCase "keep edges"
        $   s (filterVertices ("b" /=) (path ["a", "b", "c"]))
        @?= path ["a", "c"]
    , testCase "keep disconnected vertices"
        $   s (filterVertices ("b" /=) (vertices ["a", "b", "c"]))
        @?= vertices ["a", "c"]
    ]

hprop_noGraphTriangles :: Property
hprop_noGraphTriangles = property $ do
    g <- forAll $ genIntGraph (Range.linear 0 10)
    let vs = vertexList g
    []
        === [ (a, b, c)
            | a <- vs
            , b <- vs
            , c <- vs
            , hasEdge a b g && hasEdge b c g && hasEdge a c g
            ]

hprop_noGraphLoops :: Property
hprop_noGraphLoops = property $ do
    g <- forAll $ genIntGraph (Range.linear 0 10)
    let vs = vertexList g
    [] === [ a | a <- vs, hasEdge a a g ]

hprop_graphIsAcyclic :: Property
hprop_graphIsAcyclic = property $ do
    g <- forAll $ genIntGraph (Range.linear 0 10)
    Hedgehog.assert $ isAcyclic $ toGraph g

test_graphRoots :: [TestTree]
test_graphRoots =
    [testCase "graph roots" $ graphRoots (s (path ["a", "b"])) @?= ["a"]]

hprop_noIncomingToGraphRoots :: Property
hprop_noIncomingToGraphRoots = property $ do
    g <- forAll $ genIntGraph (Range.linear 0 10)
    let vs = vertexList g
    let rs = graphRoots g
    _ <- annotateShow rs
    [] === [ (a, b) | a <- vs, b <- rs, hasEdge a b g ]

genIntGraph :: MonadGen m => Range Int -> m (Graph Int)
genIntGraph r = genGraph $ (\x -> range (0, x)) <$> Gen.int r

genGraph :: (Ord a, MonadGen m) => m [a] -> m (Graph a)
genGraph genVertices = do
    vs <- genVertices
    es <- Gen.subsequence [ (a, b) | a <- vs, b <- vs ]
    return $ overlay (vertices vs) (edges es)

s :: Graph Text -> Graph Text
s = id

test_foldTopologically :: [TestTree]
test_foldTopologically =
    [ testCase "use last in the path" $ foldLast (path ["a", "b", "c"]) @?= Just
        (Set.fromList ["c"])
    , testCase "merge all vertices"
        $   foldSet (overlay (path ["a", "b", "d"]) (path ["a", "c", "d"]))
        @?= Just (Set.fromList ["a", "b", "c", "d"])
    , testCase "merge disconnected graphs"
        $   foldLast (overlay (path ["a", "b"]) (path ["c", "d"]))
        @?= Just (Set.fromList ["b", "d"])
    , testProperty "process every vertex once" propertyProcessVertexOnce
    ]
  where
    foldSet :: Graph Text -> Maybe (Set Text)
    foldSet = runIdentity . foldTopologically setVCombine setHCombine
    setVCombine x ys = pure $ Set.insert x (mconcat ys)
    setHCombine = pure . fold
    foldLast :: Graph Text -> Maybe (Set Text)
    foldLast = runIdentity . foldTopologically lastVCombine setHCombine
    lastVCombine x _ = pure $ Set.singleton x

propertyProcessVertexOnce :: Property
propertyProcessVertexOnce = property $ do
    graph <- forAll $ genIntGraph (Range.linear 0 100)
    let graphVertices = vertexList graph
    let record a _ = modify (a :) $> ()
    let ignore _ = return ()
    (_, foldedVertices) <- runStateT (foldTopologically record ignore graph) []
    sort foldedVertices === sort graphVertices

test_graphSubset :: [TestTree]
test_graphSubset =
    [ testCase "keeps parents even if dropping is prefered"
        $   graphSubset
                (\case
                    "b" -> MustKeep
                    _   -> PreferDrop
                )
                (path ["a", "b"])
        @?= path @Text ["a", "b"]
    , testCase "keeps children of kept vertices if keeping is prefered"
        $   graphSubset
                (\case
                    "a" -> MustKeep
                    _   -> PreferKeep
                )
                (path ["a", "b"])
        @?= path @Text ["a", "b"]
    , testCase "drops children of kept vertices if dropping is prefered"
        $   graphSubset
                (\case
                    "a" -> MustKeep
                    _   -> PreferDrop
                )
                (path ["a", "b"])
        @?= path @Text ["a"]
    , testCase "handle interleaving PreferKeep and PreferDrop"
        $   graphSubset
                (\case
                    "a" -> MustKeep
                    "b" -> MustDrop
                    "c" -> PreferKeep
                    "d" -> PreferKeep
                    "f" -> PreferKeep
                    _   -> PreferDrop
                )
                (path ["a", "b", "c", "d", "e", "f"])
        @?= path @Text ["a"]
    , testCase
            "keeps children of kept vertices if grandchildren dropped when keeping is prefered"
        $   graphSubset
                (\case
                    "a" -> MustKeep
                    "b" -> PreferKeep
                    "c" -> PreferDrop
                    "d" -> MustDrop
                    _   -> MustDrop
                )
                (path ["a", "b", "c", "d"])
        @?= path @Text ["a", "b"]
    , testCase "drops children of dropped vertices"
        $   graphSubset
                (\case
                    "a" -> MustDrop
                    _   -> PreferKeep
                )
                (path ["a", "b"])
        @?= path @Text []
    , testCase "drops children of both dropped and kept vertices"
        $   graphSubset
                (\case
                    "a" -> MustDrop
                    "b" -> MustKeep
                    _   -> PreferKeep
                )
                (overlay (path ["a", "c"]) (path ["b", "c"]))
        @?= path @Text ["b"]
    , testProperty "respect vertex decisions"
                   propertyGraphSubsetRespectsDecisions
    ]

propertyGraphSubsetRespectsDecisions :: Property
propertyGraphSubsetRespectsDecisions = property $ do
    graph <- forAll $ genIntGraph (Range.linear 0 100)
    let graphVertices = vertexList graph
    decisions <- forAll
        $ traverse (\v -> (v, ) <$> Gen.enumBounded) graphVertices
    let ancestors = graphAncestors
            graph
            (fst <$> filter ((==) MustKeep . snd) decisions)
    let overrides      = Map.fromList $ zip ancestors (repeat MustKeep)
    let finalDecisions = Map.union overrides (Map.fromList decisions)
    let subgraph = graphSubset
            (flip (Map.findWithDefault PreferKeep) finalDecisions)
            graph
    annotateShow subgraph
    let subgraphVertices = Set.fromList $ vertexList subgraph
    for_ (Map.toList finalDecisions) $ \case
        (v, MustDrop) -> Hedgehog.assert $ Set.notMember v subgraphVertices
        (v, MustKeep) -> Hedgehog.assert $ Set.member v subgraphVertices
        _             -> return ()
