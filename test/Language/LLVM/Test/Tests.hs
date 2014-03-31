module Language.LLVM.Test.Tests where

import Test.Tasty

import qualified Language.LLVM.Test.Constants as Constants
import qualified Language.LLVM.Test.Instructions as Instructions

tests = testGroup "language-llvm-quote" 
  [ Constants.tests
  , Instructions.tests
  ]