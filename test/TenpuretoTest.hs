module TenpuretoTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Tenpureto.Effects.Git          ( Committish(..) )
import           Tenpureto.Messages
import qualified Tenpureto.OrderedMap          as OrderedMap

import           Tenpureto

test_extractTemplateName :: [TestTree]
test_extractTemplateName =
    [ testCase "should extract from create message"
        $   extractTemplateName (commitCreateMessage "foo/bar" ["1", "2"])
        @?= Just "foo/bar"
    , testCase "should extract from update message"
        $   extractTemplateName (commitUpdateMessage "foo/bar" ["1", "2"])
        @?= Just "foo/bar"
    , testCase "should extract from create message with additional text"
        $   extractTemplateName
                (commitCreateMessage "foo/bar" ["1", "2"] <> "Foo foo foo")
        @?= Just "foo/bar"
    , testCase "should not extract from other messages"
        $   extractTemplateName "A\n\nTemplate foo/bar"
        @?= Nothing
    ]

test_extractTemplateCommits :: [TestTree]
test_extractTemplateCommits =
    [ testCase "should extract from create message"
        $   extractTemplateCommits (commitCreateMessage "foo/bar" ["1", "2"])
        @?= [Committish "1", Committish "2"]
    , testCase "should extract empty list from create message"
        $   extractTemplateCommits (commitCreateMessage "foo/bar" [])
        @?= []
    , testCase "should extract from update message"
        $   extractTemplateCommits (commitUpdateMessage "foo/bar" ["1", "2"])
        @?= [Committish "1", Committish "2"]
    , testCase "should extract from create message with additional text"
        $   extractTemplateCommits
                (commitCreateMessage "foo/bar" ["1", "2"] <> "Foo foo foo")
        @?= [Committish "1", Committish "2"]
    , testCase "should not extract from other messages"
        $   extractTemplateCommits "A\n\nTemplate foo/bar"
        @?= []
    ]

test_templateNameDefaultReplacement :: [TestTree]
test_templateNameDefaultReplacement =
    [ testCase "should map ssh git url repo name"
        $   templateNameDefaultReplacement "git@github.com:x/foo.git" "/tmp/bar"
        @?= OrderedMap.singleton "foo" "bar"
    , testCase "should map http git url repo name"
        $ templateNameDefaultReplacement "https://github.com/x/foo.git" "/tmp/bar"
        @?= OrderedMap.singleton "foo" "bar"
    , testCase "should map local git url repo name"
        $   templateNameDefaultReplacement "/tmp/foo" "/tmp/bar"
        @?= OrderedMap.singleton "foo" "bar"
    ]
