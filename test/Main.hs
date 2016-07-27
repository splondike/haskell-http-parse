{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty (testGroup, defaultMain)
import qualified Test.Data.HTTP.Parser.Header as Header
import qualified Test.Data.HTTP.Parser.Body as Body

main = defaultMain tests
   where
      tests = testGroup "Parsing tests" [Header.tests, Body.tests]
