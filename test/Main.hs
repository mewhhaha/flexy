module Main where

import Test.Tasty (defaultMain, testGroup)
import Golden (goldenTests)
import Properties (propertyTests)

main :: IO ()
main = defaultMain (testGroup "flexy-tests" [goldenTests, propertyTests])
