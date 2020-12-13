module Tenpureto.OrderedMapTest where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Applicative
import qualified Data.Map                      as Map

import           Tenpureto.OrderedMap

genUniqueListIntString :: (MonadGen m) => Range Int -> m [(Int, String)]
genUniqueListIntString range =
    (Map.assocs <$> Gen.map
            range
            (liftA2 (\a b -> (a, b))
                    (Gen.int range)
                    (Gen.string (Range.singleton 1) Gen.lower)
            )
        )
        >>= Gen.shuffle

genOrderedMapIntString
    :: (MonadGen m) => Range Int -> m (OrderedMap Int String)
genOrderedMapIntString range = fromList <$> genUniqueListIntString range

hprop_preservesElements :: Property
hprop_preservesElements = property $ do
    list <- forAll $ genUniqueListIntString (Range.linear 0 10)
    (toList . fromList) list === list

test_union :: [TestTree]
test_union =
    [ testGroup
        "preserves order"
        [ testCase "concatenation"
        $       s [(1, 'a'), (2, 'b')]
        `union` s [(2, 'c'), (3, 'd')]
        @?=     s [(1, 'a'), (2, 'c'), (3, 'd')]
        , testCase "insertion"
        $       s [(1, 'a'), (3, 'b'), (4, 'c'), (5, 'd')]
        `union` s [(2, 'e'), (3, 'f'), (4, 'g')]
        @?=     s [(1, 'a'), (2, 'e'), (3, 'f'), (4, 'g'), (5, 'd')]
        ]
    , testCase "resolves conflicts"
        $       s [(1, 'a'), (2, 'b'), (3, 'c')]
        `union` s [(3, 'd'), (4, 'e'), (1, 'f')]
        @?=     s [(2, 'b'), (3, 'd'), (4, 'e'), (1, 'f')]
    ]

test_show :: [TestTree]
test_show =
    [ testCase "shows as fromList"
          $   show (s [(1, 'a'), (2, 'b')])
          @?= "fromList [(1,'a'),(2,'b')]"
    ]

s :: [(Int, Char)] -> OrderedMap Int Char
s = fromList
