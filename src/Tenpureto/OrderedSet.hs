module Tenpureto.OrderedSet
  ( OrderedSet
  , fromList
  , toList
  , toSet
  , empty
  , intersection
  , union
  , singleton
  )
where

import           Data.Maybe                     ( fromJust )
import           Data.List                      ( sortOn
                                                , find
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map

import           Algebra.Graph.AdjacencyMap     ( path
                                                , overlay
                                                , removeEdge
                                                , hasEdge
                                                )
import           Algebra.Graph.AdjacencyMap.Algorithm
                                                ( topSort )
import           Data.Text.Prettyprint.Doc      ( Pretty
                                                , pretty
                                                , group
                                                , encloseSep
                                                )

newtype OrderedSet a = OrderedSet { toList :: [a] }
                                  deriving (Eq, Ord)

fromList :: Ord a => [a] -> OrderedSet a
fromList list = OrderedSet unique
 where
  unique = map fst (sortOn snd (Map.assocs m))
  (m, _) = foldl
    (\(acc, idx) e -> (acc `Map.union` Map.singleton e idx, idx + 1))
    (Map.empty, 0 :: Int)
    list

toSet :: Ord a => OrderedSet a -> Set a
toSet = Set.fromList . toList

empty :: OrderedSet a
empty = OrderedSet []

singleton :: a -> OrderedSet a
singleton a = OrderedSet [a]

intersection :: Ord a => OrderedSet a -> OrderedSet a -> OrderedSet a
intersection a b = OrderedSet fa `union` OrderedSet fb
 where
  fa     = filter common (toList a)
  fb     = filter common (toList b)
  common = flip Set.member u
  u      = toSet a `Set.intersection` toSet b

union :: Ord a => OrderedSet a -> OrderedSet a -> OrderedSet a
union a b = OrderedSet . sort $ ga `overlay` gb
 where
  ga = path (toList a)
  gb = path (toList b)
  sort g = case topSort g of
    Right v -> v
    Left  c -> sort $ uncurry removeEdge (cycleEdge c) g
  cycleEdges (h :| t) = zip (h : t) (tail (cycle (h : t)))
  cycleEdge = fromJust . find (not . belongsTo gb) . cycleEdges
  belongsTo = flip (uncurry hasEdge)

instance (Ord a, Show a) => Show (OrderedSet a) where
  show = show . toSet

instance (Ord a, Pretty a) => Pretty (OrderedSet a) where
  pretty s = group . encloseSep "[ " " ]" ", " $ pretty <$> toList s
