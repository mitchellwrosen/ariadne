module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Ariadne.SrcMapTest (srcMapTests)

main = defaultMain tests

tests = testGroup "Tests" [ testGroup "SrcMap tests" srcMapTests ]
