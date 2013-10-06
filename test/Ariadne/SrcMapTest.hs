module Ariadne.SrcMapTest ( srcMapTests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Exts.SrcLoc

import qualified Ariadne.SrcMap as SrcMap

srcMapTests =
  [ testCase "lookup 1" $ SrcMap.lookup (SrcLoc "" 1 3) m @?= Just ()
  , testCase "lookup 2" $ SrcMap.lookup (SrcLoc "" 1 4) m @?= Just ()
  , testCase "lookup 3" $ SrcMap.lookup (SrcLoc "" 1 5) m @?= Nothing
  , testCase "lookup 4" $ SrcMap.lookup (SrcLoc "" 1 2) m @?= Nothing
  , testCase "lookup 5" $ SrcMap.lookup (SrcLoc "" 2 3) m @?= Nothing
  ]

m = SrcMap.singleton (SrcSpan "" 1 3 1 5) ()
