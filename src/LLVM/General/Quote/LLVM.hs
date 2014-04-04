module LLVM.General.Quote.LLVM (
    llmod,
    lldef,
    llg,
    llbb,
    lli
  ) where

import qualified LLVM.General.Quote.Parser.Parser as P
import qualified LLVM.General.Quote.AST as A
import LLVM.General.Quote.Base (quasiquote, TQuasiQuoter(unTQuasiQuoter))
import Language.Haskell.TH.Quote (QuasiQuoter)

import qualified LLVM.General.AST as L

exts :: [A.Extensions]
exts = [A.Loops]

-- |Quasiquoter for 'LLVM.General.AST.Module'
llmod :: QuasiQuoter
llmod = unTQuasiQuoter
          (quasiquote exts P.parseModule :: TQuasiQuoter L.Module)

-- |Quasiquoter for 'LLVM.General.AST.Definition'
lldef :: QuasiQuoter
lldef = unTQuasiQuoter
          (quasiquote exts P.parseDefinition :: TQuasiQuoter L.Definition)

-- |Quasiquoter for 'LLVM.General.AST.Global'
llg :: QuasiQuoter
llg = unTQuasiQuoter (quasiquote exts P.parseGlobal :: TQuasiQuoter L.Global)

-- |Quasiquoter for 'LLVM.General.AST.BasicBlock'
llbb :: QuasiQuoter
llbb = unTQuasiQuoter
         (quasiquote exts P.parseBasicBlock :: TQuasiQuoter L.BasicBlock)

-- |Quasiquoter for 'LLVM.General.AST.Instruction.Instruction'
lli :: QuasiQuoter
lli = unTQuasiQuoter
        (quasiquote exts P.parseInstruction :: TQuasiQuoter L.Instruction)
