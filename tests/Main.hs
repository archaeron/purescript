{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import           Prelude        ()
import           Prelude.Compat

import Test.Tasty

import qualified TestCompiler
import qualified TestCoreFn
import qualified TestDocs
import qualified TestHierarchy
import qualified TestFormat
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestIde
import qualified TestPscPublish
import qualified TestUtils

import           System.IO      (hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode
  heading "Prim documentation test suite"
  TestPrimDocs.main

  ideTests <- TestIde.main
  compilerTests <- TestCompiler.main
  psciTests <- TestPsci.main
  coreFnTests <- TestCoreFn.main
  docsTests <- TestDocs.main
  publishTests <- TestPscPublish.main
  hierarchyTests <- TestHierarchy.main
  formatTest <- TestFormat.main

  defaultMain $
    testGroup
      "Tests"
      [ compilerTests
      , psciTests
      , ideTests
      , coreFnTests
      , docsTests
      , publishTests
      , hierarchyTests
      , formatTest
      ]

  where
  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
