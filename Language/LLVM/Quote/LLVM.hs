module Language.LLVM.Quote.LLVM (
    llmod
  ) where

import qualified Language.LLVM.Parser as P
import qualified Language.LLVM.AST as A
import Language.LLVM.Quote.Base (quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [A.Extensions]
exts = []

llmod :: QuasiQuoter
llmod = quasiquote exts P.parseModule
