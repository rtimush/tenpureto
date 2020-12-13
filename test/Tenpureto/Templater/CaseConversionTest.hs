{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Tenpureto.Templater.CaseConversionTest where

import           Test.SmallCheck
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Attoparsec.Text
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                     as T
import           GHC.Generics

import           Tenpureto.Templater.CaseConversion

assertElem :: (Show a, Eq a) => a -> [a] -> Assertion
assertElem a b = assertBool
    ("expected: " ++ show a ++ " to be an element of: " ++ show b)
    (a `elem` b)

assertNotElem :: (Show a, Eq a) => a -> [a] -> Assertion
assertNotElem a b = assertBool
    ("expected: " ++ show a ++ " not to be an element of: " ++ show b)
    (a `notElem` b)

scprop_wordCasePatternsNotEmpty :: WordCase -> Bool
scprop_wordCasePatternsNotEmpty wc =
    isNothing $ maybeResult $ parse (wordCasePattern wc) ""

test_detectValueStyle :: [TestTree]
test_detectValueStyle =
    fmap runTest cases ++ fmap runNegativeTest negativeCases
  where
    runTest (str, tw) =
        let values = textToTemplateValues str
        in  testCase (T.unpack str <> " is a " <> show tw)
                     (tw `assertElem` values)
    runNegativeTest (str, tw) =
        let values = textToTemplateValues str
        in  testCase (T.unpack str <> " is not a " <> show tw)
                     (tw `assertNotElem` values)
    cases =
        [ ( "aaaBbbCcc"
          , MultiWordTemplateValue LowerCase
                                   TitleCase
                                   NoSeparator
                                   ["aaa", "Bbb", "Ccc"]
          )
        , ( "ыыыЮююФфф"
          , MultiWordTemplateValue LowerCase
                                   TitleCase
                                   NoSeparator
                                   ["ыыы", "Ююю", "Ффф"]
          )
        , ( "AaaBbbCcc"
          , MultiWordTemplateValue TitleCase
                                   TitleCase
                                   NoSeparator
                                   ["Aaa", "Bbb", "Ccc"]
          )
        , ( "aaa-bbb-ccc"
          , MultiWordTemplateValue LowerCase
                                   LowerCase
                                   (SingleSeparator '-')
                                   ["aaa", "bbb", "ccc"]
          )
        , ( "aa0-bbb-ccc"
          , MultiWordTemplateValue MixedCase
                                   LowerCase
                                   (SingleSeparator '-')
                                   ["aa0", "bbb", "ccc"]
          )
        , ( "AAA_BBB_CCC"
          , MultiWordTemplateValue UpperCase
                                   UpperCase
                                   (SingleSeparator '_')
                                   ["AAA", "BBB", "CCC"]
          )
        , ( "Aaa bbb ccc"
          , MultiWordTemplateValue TitleCase
                                   LowerCase
                                   (SingleSeparator ' ')
                                   ["Aaa", "bbb", "ccc"]
          )
        , ( "aaa/bbb/ccc"
          , MultiWordTemplateValue LowerCase
                                   LowerCase
                                   (SingleSeparator '/')
                                   ["aaa", "bbb", "ccc"]
          )
        , ( "aaa\\bbb\\ccc"
          , MultiWordTemplateValue LowerCase
                                   LowerCase
                                   (SingleSeparator '\\')
                                   ["aaa", "bbb", "ccc"]
          )
        , ( "aaa.bbb.ccc"
          , MultiWordTemplateValue LowerCase
                                   LowerCase
                                   (SingleSeparator '.')
                                   ["aaa", "bbb", "ccc"]
          )
        , ("aaabbbccc", SingleWordTemplateValue LowerCase "aaabbbccc")
        , ("Aaabbbccc", SingleWordTemplateValue TitleCase "Aaabbbccc")
        , ("Aa0"      , SingleWordTemplateValue MixedCase "Aa0")
        , ("Aa0"      , SingleWordTemplateValue TitleCase "Aa0")
        , ("aa0"      , SingleWordTemplateValue MixedCase "aa0")
        , ("aa0"      , SingleWordTemplateValue LowerCase "aa0")
        ]
    negativeCases =
        [ ( "AAA"
          , MultiWordTemplateValue TitleCase
                                   TitleCase
                                   NoSeparator
                                   ["A", "A", "A"]
          )
        ]

scprop_templateValueTextReversible :: TestText -> Either Reason Reason
scprop_templateValueTextReversible testText =
    let text   = T.pack $ show testText
        values = textToTemplateValues text
        reversible value =
            let reversed = templateValueText value
            in  if reversed == text
                    then Right ()
                    else Left
                        (  T.unpack reversed
                        ++ " is not equal to "
                        ++ T.unpack text
                        ++ " when represented as "
                        ++ show value
                        )
        results = traverse_ reversible values
    in  results >> return (T.unpack text ++ " is reversible")

scprop_multiWordTemplateValueLength :: TestText -> Either Reason Reason
scprop_multiWordTemplateValueLength testText =
    let text   = T.pack $ show testText
        values = textToTemplateValues text
        valid (MultiWordTemplateValue fw ow ss tt) = if length tt >= 2
            then Right ()
            else Left
                (  show (MultiWordTemplateValue fw ow ss tt)
                <> " has only "
                <> show (length tt)
                <> " word"
                )
        valid _ = Right ()
        results = traverse_ valid values
    in  results >> return (T.unpack text ++ " is reversible")

instance Monad m => Serial m WordCase

newtype TestChar = TestChar { testChar :: Char }

instance Monad m => Serial m TestChar where
    series = generate
        (const $ map TestChar (['a', 'A', '1', '!'] ++ validSeparators))

newtype TestText = TestText { testChars :: [TestChar] } deriving (Generic)

instance Monad m => Serial m TestText
instance Show TestText where
    show testText = map testChar (testChars testText)
