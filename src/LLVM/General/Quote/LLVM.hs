module LLVM.General.Quote.LLVM (
    llmod,
    lldef,
    llbb,
    lli
  ) where

import qualified LLVM.General.Quote.Parser.Parser as P
import qualified LLVM.General.Quote.AST as A
import LLVM.General.Quote.Base (quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [A.Extensions]
exts = [A.Loops]

-- |Quasiquoter for 'LLVM.General.AST.Module'
llmod :: QuasiQuoter
llmod = quasiquote exts P.parseModule

-- |Quasiquoter for 'LLVM.General.AST.Definition'
lldef :: QuasiQuoter
lldef = quasiquote exts P.parseDefinition

-- |Quasiquoter for 'LLVM.General.AST.BasicBlock'
llbb :: QuasiQuoter
llbb = quasiquote exts P.parseBasicBlock

-- |Quasiquoter for 'LLVM.General.AST.Instruction.Instruction'
lli :: QuasiQuoter
lli = quasiquote exts P.parseInstruction
