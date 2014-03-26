module Language.LLVM.Quote.LLVM (
    llmod,
    lldef,
    llbb,
    lli
  ) where

import qualified Language.LLVM.Parser as P
import qualified Language.LLVM.AST as A
import Language.LLVM.Quote.Base (quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [A.Extensions]
exts = [A.Loops]

llmod :: QuasiQuoter
llmod = quasiquote exts P.parseModule

lldef :: QuasiQuoter
lldef = quasiquote exts P.parseDefinition

llbb :: QuasiQuoter
llbb = quasiquote exts P.parseBasicBlock

lli :: QuasiQuoter
lli = quasiquote exts P.parseInstruction
