{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.Effects.GitTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Path
import           Tenpureto.Effects.Git

test_parseRepositoryUri :: [TestTree]
test_parseRepositoryUri =
    [ test "should parse org/repo"
           "f_o-o/b_a-r.r"
           (OwnerRepoRepository "f_o-o" "b_a-r.r")
    , test "should parse scp-like git uri"
           "git@github.com:foo/bar.git"
           (RemoteRepository "git@github.com:foo/bar.git")
    , test "should parse ssh uri"
           "ssh://git@github.com/foo/bar.git"
           (RemoteRepository "ssh://git@github.com/foo/bar.git")
    , test "should parse git uri"
           "git://git@github.com/foo/bar.git"
           (RemoteRepository "git://git@github.com/foo/bar.git")
    , test "should parse https uri"
           "https://git@github.com/foo/bar.git"
           (RemoteRepository "https://git@github.com/foo/bar.git")
    , test "should parse file uri"
           "file:///foo/bar"
           (RemoteRepository "file:///foo/bar")
    , test "should parse unix file path"
           "/foo/bar"
           (LocalRepository [absdir|/foo/bar|])
    ]
    where test n u r = testCase n (parseRepositoryUri u @?= r)
