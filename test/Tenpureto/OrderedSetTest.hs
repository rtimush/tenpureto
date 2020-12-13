module Tenpureto.OrderedSetTest where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           Data.List                      ( isSubsequenceOf
                                                , sort
                                                )
import qualified Data.Set                      as Set

import           Tenpureto.OrderedSet

genUniqueList :: (MonadGen m, Eq a, Ord a) => Range Int -> m a -> m [a]
genUniqueList range gen = (Set.toList <$> Gen.set range gen) >>= Gen.shuffle

genOrderedSet
    :: (MonadGen m, Eq a, Ord a) => Range Int -> m a -> m (OrderedSet a)
genOrderedSet range gen = fromList <$> genUniqueList range gen

genUniqueListInt :: MonadGen m => Range Int -> m [Int]
genUniqueListInt range = genUniqueList range (Gen.int range)

genOrderedSetInt :: MonadGen m => Range Int -> m (OrderedSet Int)
genOrderedSetInt range = genOrderedSet range (Gen.int range)

hprop_preservesElements :: Property
hprop_preservesElements = property $ do
    list <- forAll
        $ genUniqueList (Range.linear 0 10) (Gen.int Range.constantBounded)
    (toList . fromList) list === list

test_intersection :: [TestTree]
test_intersection =
    [ testGroup
        "preserves order"
        [ testCase "infix" $ s [1, 2, 3, 4] `intersection` s [2, 3] @?= s [2, 3]
        , testCase "subsequence"
        $              s [1, 2, 3, 5]
        `intersection` s [2, 4, 5, 6]
        @?=            s [2, 5]
        ]
    , testCase "resolves conflicts"
        $              s [1, 2, 3]
        `intersection` s [3, 2, 4]
        @?=            s [3, 2]
    , testProperty "preserves elements" intersectionPreservesElements
    , testProperty "preserves right-side order"
                   intersectionPreservesRightSideOrder
    ]

intersectionPreservesElements :: Property
intersectionPreservesElements = property $ do
    a <- forAll $ genUniqueListInt (Range.linear 0 10)
    b <- forAll $ genUniqueListInt (Range.linear 0 10)
    let elements = toList (fromList a `intersection` fromList b)
    let expected =
            Set.toList (Set.fromList a `Set.intersection` Set.fromList b)
    sort elements === expected

intersectionPreservesRightSideOrder :: Property
intersectionPreservesRightSideOrder = property $ do
    a <- forAll $ genUniqueListInt (Range.linear 0 10)
    b <- forAll $ genUniqueListInt (Range.linear 0 10)
    let elements = toList (fromList a `intersection` fromList b)
    diff elements isSubsequenceOf b

test_union :: [TestTree]
test_union =
    [ testGroup
        "preserves order"
        [ testCase "concatenation" $ s [1, 2] `union` s [2, 3] @?= s [1, 2, 3]
        , testCase "insertion" $ s [1, 3, 4, 5] `union` s [2, 3, 4] @?= s
            [1, 2, 3, 4, 5]
        ]
    , testCase "resolves conflicts" $ s [1, 2, 3] `union` s [3, 4, 1] @?= s
        [2, 3, 4, 1]
    , testProperty "preserves elements"         unionPreservesElements
    , testProperty "preserves right-side order" unionPreservesRightSideOrder
    ]

unionPreservesElements :: Property
unionPreservesElements = property $ do
    a <- forAll $ genUniqueListInt (Range.linear 0 10)
    b <- forAll $ genUniqueListInt (Range.linear 0 10)
    let elements = toList (fromList a `union` fromList b)
    let expected = Set.toList (Set.fromList a <> Set.fromList b)
    sort elements === expected

unionPreservesRightSideOrder :: Property
unionPreservesRightSideOrder = property $ do
    a <- forAll $ genUniqueListInt (Range.linear 0 10)
    b <- forAll $ genUniqueListInt (Range.linear 0 10)
    let elements = toList (fromList a `union` fromList b)
    diff b isSubsequenceOf elements

test_show :: [TestTree]
test_show =
    [testCase "shows as fromList" $ show (s [1, 2]) @?= "fromList [1,2]"]

s :: [Int] -> OrderedSet Int
s = fromList
