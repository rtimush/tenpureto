{-# LANGUAGE DeriveGeneric #-}

module Tenpureto.Templater.CaseConversion where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Char
import           Data.Either.Combinators        ( rightToMaybe )
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics

data WordCase = LowerCase | UpperCase | TitleCase | MixedCase
    deriving (Show, Eq, Ord, Generic)

data WordSeparator =  NoSeparator | SingleSeparator Char
    deriving (Show, Eq, Ord)

data TemplateValue = MultiWordTemplateValue WordCase WordCase WordSeparator [Text]
                   | SingleWordTemplateValue WordCase Text
                   | LiteralTemplateValue Text
    deriving (Show, Eq, Ord)

allWordCases :: [WordCase]
allWordCases = [LowerCase, UpperCase, TitleCase, MixedCase]

validSeparators :: String
validSeparators = [' ', '-', '_', '/', '\\', '.']

allWordSeparators :: [WordSeparator]
allWordSeparators = NoSeparator : [ SingleSeparator c | c <- validSeparators ]

wordCasePattern :: WordCase -> Parser Text
wordCasePattern LowerCase = takeWhile1 (\c -> isLower c || isDigit c)
wordCasePattern UpperCase = takeWhile1 (\c -> isUpper c || isDigit c)
wordCasePattern TitleCase =
    T.cons <$> satisfy isUpper <*> takeWhile1 (\c -> isLower c || isDigit c)
wordCasePattern MixedCase = takeWhile1 isAlphaNum

wordCaseApply :: WordCase -> Text -> Text
wordCaseApply LowerCase = T.toLower
wordCaseApply UpperCase = T.toUpper
wordCaseApply TitleCase =
    uncurry (<>) . bimap T.toUpper T.toLower . T.splitAt 1
wordCaseApply MixedCase = id

wordCasesApply :: WordCase -> WordCase -> [Text] -> [Text]
wordCasesApply fwc owc =
    zipWith ($) (wordCaseApply fwc : repeat (wordCaseApply owc))

separator :: WordSeparator -> Text
separator NoSeparator         = ""
separator (SingleSeparator c) = T.singleton c

multiWordPatterns :: [Parser TemplateValue]
multiWordPatterns =
    [ MultiWordTemplateValue fwc owc s <$> build fwc owc s
    | fwc <- allWordCases
    , owc <- allWordCases
    , s   <- allWordSeparators
    ]
  where
    build fwc owc s = (:) <$> wordCasePattern fwc <*> some
        (string (separator s) *> wordCasePattern owc)

singleWordPatterns :: [Parser TemplateValue]
singleWordPatterns =
    [ SingleWordTemplateValue c <$> wordCasePattern c | c <- allWordCases ]

textToTemplateValues :: Text -> [TemplateValue]
textToTemplateValues text =
    LiteralTemplateValue text
        :  mapMaybe f singleWordPatterns
        ++ mapMaybe f multiWordPatterns
    where f p = rightToMaybe $ parseOnly (p <* endOfInput) text

templateValueText :: TemplateValue -> Text
templateValueText (LiteralTemplateValue t     ) = t
templateValueText (SingleWordTemplateValue c t) = wordCaseApply c t
templateValueText (MultiWordTemplateValue fwc owc s tt) =
    T.intercalate (separator s) (wordCasesApply fwc owc tt)

sameStyle :: TemplateValue -> TemplateValue -> Bool
sameStyle (MultiWordTemplateValue fwc owc s _) (MultiWordTemplateValue fwc' owc' s' _)
    = fwc == fwc' && owc == owc' && s == s'
sameStyle (MultiWordTemplateValue fwc _ _ _) (SingleWordTemplateValue c' _) =
    fwc == c'
sameStyle (SingleWordTemplateValue c _) (SingleWordTemplateValue c' _) =
    c == c'
sameStyle (LiteralTemplateValue _) (LiteralTemplateValue _) = True
sameStyle _                        _                        = False

wordCaseVariations :: WordCase -> [WordCase]
wordCaseVariations MixedCase = [LowerCase, UpperCase, TitleCase, MixedCase]
wordCaseVariations _         = [LowerCase, UpperCase, TitleCase]

styleVariations :: TemplateValue -> [TemplateValue]
styleVariations (MultiWordTemplateValue fwc owc _ tt) =
    [ MultiWordTemplateValue fwc' owc' s' (wordCasesApply fwc' owc' tt)
    | fwc' <- wordCaseVariations fwc
    , owc' <- wordCaseVariations owc
    , s'   <- allWordSeparators
    ]
styleVariations (SingleWordTemplateValue c t) =
    LiteralTemplateValue t
        : [ SingleWordTemplateValue c' (wordCaseApply c' t)
          | c' <- wordCaseVariations c
          ]
styleVariations (LiteralTemplateValue t) = [LiteralTemplateValue t]
