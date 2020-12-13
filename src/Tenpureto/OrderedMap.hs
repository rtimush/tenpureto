{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Tenpureto.OrderedMap
    ( OrderedMap
    , toList
    , fromList
    , mapWithKey
    , empty
    , singleton
    , elems
    , union
    , Tenpureto.OrderedMap.lookup
    , intersectionWith
    ) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc      ( Pretty
                                                , encloseSep
                                                , group
                                                , pretty
                                                )
import           Data.Yaml.Builder              ( (.=)
                                                , ToYaml(..)
                                                , mapping
                                                )
import           Data.Yaml.Parser               ( FromYaml(..)
                                                , YamlParser
                                                , YamlValue
                                                , withMapping
                                                )
import           Tenpureto.OrderedSet           ( OrderedSet )
import qualified Tenpureto.OrderedSet          as OrderedSet

data OrderedMap a b = OrderedMap
    { keySet :: OrderedSet a
    , toMap  :: Map a b
    }
    deriving (Eq, Ord)

fromList :: (Eq a, Ord a) => [(a, b)] -> OrderedMap a b
fromList kv = OrderedMap { keySet = OrderedSet.fromList (fmap fst kv)
                         , toMap  = Map.fromList kv
                         }

toList :: Ord k => OrderedMap k v -> [(k, v)]
toList m = (\k -> (k, m ! k)) <$> OrderedSet.toList (keySet m)

(!) :: Ord k => OrderedMap k a -> k -> a
(!) m = (Map.!) (toMap m)

mapWithKey :: (k -> a -> b) -> OrderedMap k a -> OrderedMap k b
mapWithKey f m =
    OrderedMap { keySet = keySet m, toMap = Map.mapWithKey f (toMap m) }

empty :: OrderedMap k a
empty = OrderedMap { keySet = OrderedSet.empty, toMap = Map.empty }

singleton :: k -> a -> OrderedMap k a
singleton k v =
    OrderedMap { keySet = OrderedSet.singleton k, toMap = Map.singleton k v }

elems :: Ord k => OrderedMap k a -> [a]
elems = fmap snd . toList

lookup :: Ord k => k -> OrderedMap k a -> Maybe a
lookup k m = Map.lookup k (toMap m)

intersectionWith
    :: Ord k
    => (a -> b -> c)
    -> OrderedMap k a
    -> OrderedMap k b
    -> OrderedMap k c
intersectionWith f a b = OrderedMap
    { keySet = OrderedSet.intersection (keySet a) (keySet b)
    , toMap  = Map.intersectionWith f (toMap a) (toMap b)
    }

union :: Ord k => OrderedMap k v -> OrderedMap k v -> OrderedMap k v
union a b = OrderedMap { keySet = OrderedSet.union (keySet a) (keySet b)
                       , toMap  = Map.union (toMap b) (toMap a)
                       }

instance FromYaml a => FromYaml (OrderedMap Text a) where
    fromYaml = withMapping "Map" (fmap fromList . mapM value)
      where
        value :: (Text, YamlValue) -> YamlParser (Text, a)
        value (k, v) = fmap (k, ) (fromYaml v)

instance ToYaml a => ToYaml (OrderedMap Text a) where
    toYaml = mapping . map (uncurry (.=)) . toList

instance Functor (OrderedMap k) where
    fmap f m = OrderedMap { keySet = keySet m, toMap = fmap f (toMap m) }

instance (Ord a, Pretty a, Pretty b) => Pretty (OrderedMap a b) where
    pretty s = group . encloseSep "{ " " }" ", " $ pretty <$> toList s

instance (Ord a, Show a, Show b) => Show (OrderedMap a b) where
    show = show . toMap
