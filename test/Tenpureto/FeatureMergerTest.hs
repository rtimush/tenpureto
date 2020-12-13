module Tenpureto.FeatureMergerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Algebra.Graph.ToGraph
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )

import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.FeatureMerger
import           Tenpureto.Graph
import qualified Tenpureto.OrderedMap          as OrderedMap
import qualified Tenpureto.OrderedSet          as OrderedSet
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                )
import           Tenpureto.TemplateLoader.Internal
                                                ( FeatureStability(..)
                                                , TemplateYaml(..)
                                                , TemplateYamlFeature(..)
                                                )


v :: Text -> TemplateBranchInformation
v c = TemplateBranchInformation
    { branchName   = c
    , branchCommit = Committish c
    , templateYaml = TemplateYaml
                         { yamlVariables = OrderedMap.singleton c c
                         , yamlExcludes  = Set.singleton c
                         , yamlConflicts = Set.singleton c
                         , yamlFeatures  = Set.singleton $ TemplateYamlFeature
                                               { yamlFeatureName        = c
                                               , yamlFeatureDescription = Nothing
                                               , yamlFeatureHidden      = False
                                               , yamlFeatureStability   = Stable
                                               }
                         }
    }

test_runMergeGraphPure :: [TestTree]
test_runMergeGraphPure =
    [ testGroup
        "merge order"
        [ testCase "path"
        $   fst (runMergeGraphPure' (path [v "a", v "b", v "c"]))
        @?= [CheckoutRecord "c"]
        , testCase "diamond"
        $   fst
                (runMergeGraphPure'
                    (overlay (path [v "a", v "b", v "d"])
                             (path [v "a", v "c", v "d"])
                    )
                )
        @?= [CheckoutRecord "d"]
        , testCase "fork"
        $   fst
                (runMergeGraphPure'
                    (overlay (path [v "a", v "b"]) (path [v "a", v "c"]))
                )
        @?= [MergeRecord "b" "c" "b+c", CheckoutRecord "b+c"]
        , testCase "thee unrelated braches"
        $   fst (runMergeGraphPure' (vertices [v "a", v "b", v "c"]))
        @?= [ MergeRecord "a"   "b" "a+b"
            , MergeRecord "a+b" "c" "a+b+c"
            , CheckoutRecord "a+b+c"
            ]
        ]
    , testGroup
        "merged yaml data"
        [ testCase "features"
        $   (fmap (Set.map yamlFeatureName . yamlFeatures) . yamlResult)
                (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (Set.fromList ["a", "b"])
        , testCase "variables"
        $   (fmap yamlVariables . yamlResult)
                (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (OrderedMap.fromList [("a", "a"), ("b", "b")])
        , testCase "conflicts for different vertices"
        $   (fmap yamlConflicts . yamlResult)
                (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (Set.fromList ["a", "b"])
        , testCase "conflicts for a path"
        $   (fmap yamlConflicts . yamlResult)
                (runMergeGraphPure' (path [v "a", v "b"]))
        @?= Just (Set.fromList ["b"])
        ]
    , testGroup
        "merge heads"
        [ testCase "for two different vertices"
        $   headsResult (runMergeGraphPure' (vertices [v "a", v "b"]))
        @?= Just (OrderedSet.fromList [Committish "a", Committish "b"])
        , testCase "for a path"
        $   headsResult (runMergeGraphPure' (path [v "a", v "b"]))
        @?= Just (OrderedSet.fromList [Committish "b"])
        ]
    , testCase "cycle"
        $   runMergeGraphPure' (path [v "a", v "b", v "c", v "a"])
        @?= ([], Nothing)
    ]
  where
    runMergeGraphPure' graph = runMergeGraphPure graph (vertexSet graph)
    yamlResult  = fmap fst . snd
    headsResult = fmap snd . snd
