module Language.LLVM.Test.Tests where

import Test.Tasty

import qualified Language.LLVM.Test.Constants as Constants
import qualified Language.LLVM.Test.DataLayout as DataLayout
import qualified Language.LLVM.Test.Instructions as Instructions
import qualified Language.LLVM.Test.Metadata as Metadata

tests = testGroup "language-llvm-quote" 
  [ Constants.tests
  , DataLayout.tests
  , Instructions.tests
  , Metadata.tests
  ]