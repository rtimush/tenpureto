module Tenpureto.TemplateLoaderTest where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Applicative
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Tenpureto.Graph
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.TemplateLoader
import           Tenpureto.TemplateLoader.Internal
import           Tenpureto.TemplateTestHelper

test_managedBranches :: [TestTree]
test_managedBranches =
    [ testCase "include base branches"
        $   managedBranches (templateInformation [a])
        @?= [a]
    , testCase "include child branches"
        $   managedBranches (templateInformation [a, b])
        @?= [a, b]
    , testCase "not include renamed branches"
        $   managedBranches (templateInformation [a, b, c])
        @?= [a, b]
    , testCase "include merge branches"
        $   managedBranches (templateInformation [a, b, d, e])
        @?= [a, b, d, e]
    ]
  where
    a = baseBranch "a"
    b = childBranch "b" [a]
    c = renamedBranch "c" b
    d = childBranch "d" [a]
    e = mergeBranch "e" [b, d]

test_getBranchParents :: [TestTree]
test_getBranchParents =
    [ testCase "include parents"
        $   getBranchParents (templateInformation [a, b]) b
        @?= Set.fromList ["a"]
    , testCase "include parents with additional branches"
        $   getBranchParents (templateInformation [a', b]) b
        @?= Set.fromList ["a"]
    , testCase "not include grand parents"
        $   getBranchParents (templateInformation [a, b, c]) c
        @?= Set.fromList ["b"]
    , testCase "not include renamed parents"
        $   getBranchParents (templateInformation [a, b, f, c]) c
        @?= Set.fromList ["b"]
    , testCase "not include anonymous parents"
        $   getBranchParents (templateInformation [g, a]) a
        @?= Set.fromList []
    , testCase "not include an original branch for a renamed one"
        $   getBranchParents (templateInformation [a, b, f]) f
        @?= Set.fromList ["a"]
    , testCase "not include a renamed branch for an original one"
        $   getBranchParents (templateInformation [a, b, f]) b
        @?= Set.fromList ["a"]
    ]
  where
    a  = baseBranch "a"
    p  = baseBranch "p"
    a' = childBranch "a" [p]
    b  = childBranch "b" [a]
    c  = childBranch "c" [b]
    f  = renamedBranch "f" b
    g  = anonymousBranch "g" []

test_getBranchChildren :: [TestTree]
test_getBranchChildren =
    [ testCase "include children"
        $   getBranchChildren (templateInformation [a, b]) a
        @?= Set.fromList ["b"]
    , testCase "include children with smaller feature lists"
        $   getBranchChildren (templateInformation [a', b]) a'
        @?= Set.fromList ["b"]
    , testCase "not include renamed children"
        $   getBranchChildren (templateInformation [a, b, g]) a
        @?= Set.fromList ["b"]
    , testCase "include merges"
        $   getBranchChildren (templateInformation [a, e, f]) a
        @?= Set.fromList ["f"]
    , testCase "not include grand children"
        $   getBranchChildren (templateInformation [a, b, c]) a
        @?= Set.fromList ["b"]
    , testCase "not include an original branch children for a renamed one"
        $   getBranchChildren (templateInformation [a, b, d]) d
        @?= Set.fromList []
    , testCase "not include a renamed one for an original one"
        $   getBranchChildren (templateInformation [a, b, d]) a
        @?= Set.fromList ["b"]
    ]
  where
    a  = baseBranch "a"
    p  = baseBranch "p"
    a' = childBranch "a" [p]
    b  = childBranch "b" [a]
    c  = childBranch "c" [a, b]
    d  = renamedBranch "d" a
    e  = baseBranch "e"
    f  = mergeBranch "f" [a, e]
    g  = renamedBranch "g" b

test_getTemplateBranches :: [TestTree]
test_getTemplateBranches =
    [ testCase "list child branches"
        $   getTemplateBranches (BranchFilterChildOf "a")
                                (templateInformation [a, b, c])
        @?= [b]
    , testCase "list parent branches"
        $   getTemplateBranches (BranchFilterParentOf "c")
                                (templateInformation [a, b, c])
        @?= [b]
    , testCase "apply multiple filters"
        $   getTemplateBranches
                (BranchFilterAnd
                    [BranchFilterChildOf "a", BranchFilterParentOf "c"]
                )
                (templateInformation [a, b, c, d])
        @?= [b]
    ]
  where
    a = baseBranch "a"
    b = childBranch "b" [a]
    d = childBranch "d" [a]
    e = baseBranch "e"
    c = childBranch "c" [b, e]

test_parseTemplateYaml :: [TestTree]
test_parseTemplateYaml =
    [ testCase "parse variables"
        $   parseTemplateYaml "variables: { \"Key\": \"value\" }"
        @?= Right TemplateYaml
                { yamlVariables = OrderedMap.singleton "Key" "value"
                , yamlFeatures  = mempty
                , yamlExcludes  = mempty
                , yamlConflicts = mempty
                }
    , testCase "parse excludes"
        $   parseTemplateYaml "excludes: [ \".*\" ]"
        @?= Right TemplateYaml { yamlVariables = OrderedMap.empty
                               , yamlFeatures  = mempty
                               , yamlExcludes  = Set.singleton ".*"
                               , yamlConflicts = mempty
                               }
    , testCase "parse simple features"
        $   parseTemplateYaml "features: [ \"a\" ]"
        @?= Right TemplateYaml
                { yamlVariables = OrderedMap.empty
                , yamlFeatures  = Set.singleton TemplateYamlFeature
                                      { yamlFeatureName        = "a"
                                      , yamlFeatureDescription = Nothing
                                      , yamlFeatureHidden      = False
                                      , yamlFeatureStability   = Stable
                                      }
                , yamlExcludes  = mempty
                , yamlConflicts = mempty
                }
    , testCase "parse extended features"
        $   parseTemplateYaml
                "features: [ a: { description: foo, hidden: true, stability: experimental } ]"
        @?= Right TemplateYaml
                { yamlVariables = OrderedMap.empty
                , yamlFeatures  = Set.singleton TemplateYamlFeature
                                      { yamlFeatureName        = "a"
                                      , yamlFeatureDescription = Just "foo"
                                      , yamlFeatureHidden      = True
                                      , yamlFeatureStability   = Experimental
                                      }
                , yamlExcludes  = mempty
                , yamlConflicts = mempty
                }
    , testCase "parse nulls"
        $   parseTemplateYaml "variables: { \"Key\": }"
        @?= Right TemplateYaml { yamlVariables = OrderedMap.singleton "Key" ""
                               , yamlFeatures  = mempty
                               , yamlExcludes  = mempty
                               , yamlConflicts = mempty
                               }
    , testCase "preserve variables order"
        $   parseTemplateYaml
                "variables: { \"a\": \"1\", \"c\": \"3\", \"b\": \"2\" }"
        @?= Right TemplateYaml
                { yamlVariables = OrderedMap.fromList
                                      [("a", "1"), ("c", "3"), ("b", "2")]
                , yamlFeatures  = mempty
                , yamlExcludes  = mempty
                , yamlConflicts = mempty
                }
    ]

test_formatTemplateYaml :: [TestTree]
test_formatTemplateYaml =
    [ testCase "format simple features"
        $   formatTemplateYaml
                (TemplateYaml
                    { yamlVariables = OrderedMap.empty
                    , yamlFeatures  = Set.singleton TemplateYamlFeature
                                          { yamlFeatureName        = "a"
                                          , yamlFeatureDescription = Nothing
                                          , yamlFeatureHidden      = False
                                          , yamlFeatureStability   = Stable
                                          }
                    , yamlExcludes  = mempty
                    , yamlConflicts = mempty
                    }
                )
        @?= "features:\n- a\n"
    , testCase "format extended features"
        $ formatTemplateYaml
              (TemplateYaml
                  { yamlVariables = OrderedMap.empty
                  , yamlFeatures  = Set.singleton TemplateYamlFeature
                                        { yamlFeatureName        = "a"
                                        , yamlFeatureDescription = Just "foo"
                                        , yamlFeatureHidden      = False
                                        , yamlFeatureStability   = Experimental
                                        }
                  , yamlExcludes  = mempty
                  , yamlConflicts = mempty
                  }
              )
        @?= "features:\n- a:\n    description: foo\n    stability: experimental\n"
    ]

test_buildGraph :: [TestTree]
test_buildGraph =
    [ testCase "simple graph" $ nameGraph [a, b] @?= edge "a" "b"
    , testCase "remove transitive edges" $ nameGraph [a, b, c] @?= path
        ["a", "b", "c"]
    , testCase "remove unknown vertices" $ nameGraph [b, c] @?= edge "b" "c"
    ]
  where
    a         = baseBranch "a"
    b         = childBranch "b" [a]
    c         = childBranch "c" [b]
    nameGraph = mapVertices branchName . buildGraph

genTemplateYaml :: Range Int -> Gen TemplateYaml
genTemplateYaml range =
    let smallRange   = Range.constant 0 2
        genText      = Gen.text (Range.singleton 1) Gen.alphaNum
        genTextTuple = liftA2 (\a -> \b -> (a, b)) genText genText
    in  do
            features  <- Gen.set range (genTemplateYamlFeature range)
            variables <- Gen.list smallRange genTextTuple
            excludes  <- Gen.set smallRange genText
            conflicts <- Gen.set smallRange genText
            return $ TemplateYaml
                { yamlVariables = OrderedMap.fromList variables
                , yamlFeatures  = features
                , yamlExcludes  = excludes
                , yamlConflicts = conflicts
                }

genTemplateYamlFeature :: Range Int -> Gen TemplateYamlFeature
genTemplateYamlFeature range = do
    featureId   <- Gen.int range
    description <- Gen.element [Nothing, Just (featureName featureId)]
    hidden      <- Gen.bool
    stability   <- Gen.enumBounded
    return $ TemplateYamlFeature { yamlFeatureName = featureName featureId
                                 , yamlFeatureDescription = description
                                 , yamlFeatureHidden      = hidden
                                 , yamlFeatureStability   = stability
                                 }
  where
    featureName :: Int -> Text
    featureName = (<>) "f" . T.pack . show
