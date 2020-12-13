module Tenpureto.MergeOptimizerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Functor.Identity
import           Data.Text                      ( Text )

import           Tenpureto.Graph
import           Tenpureto.MergeOptimizer
import qualified Tenpureto.OrderedMap          as OrderedMap


test_mergeGraph :: [TestTree]
test_mergeGraph =
    [ testCase "not merge parents into children"
        $ let graph = edge (v 1 "1") (v 2 "2")
          in  runIdentity (mergeGraph mergeCommits graph) @?= Just (v 2 "2")
    , testCase "merge siblings"
        $ let graph = overlay (vertex $ v 1 "1") (vertex $ v 2 "2")
          in  runIdentity (mergeGraph mergeCommits graph) @?= Just (v 3 "1+2")
    ]
  where
    mergeCommits c1 c2 _ = return (c1 + c2)
    v :: Int -> Text -> MergedBranchInformation Int
    v x n = MergedBranchInformation
        { mergedBranchMeta       = x
        , mergedBranchDescriptor = MergedBranchDescriptor
                                       { mergedBranchName = n
                                       , mergedVariables  = OrderedMap.empty
                                       , mergedExcludes   = mempty
                                       , mergedConflicts  = mempty
                                       , mergedFeatures   = mempty
                                       }
        }
