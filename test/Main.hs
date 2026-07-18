module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Examples (exampleTests)
import Properties (propertyTests)

main :: IO ()
main = defaultMain (testGroup "flexy" [exampleTests, propertyTests])
