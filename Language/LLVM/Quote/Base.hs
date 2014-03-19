{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LLVM.Quote.Base (
    ToDefintions(..),
    quasiquote
  ) where

import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as B
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Loc
import Data.Typeable (Typeable)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToExpQ)

import qualified Language.LLVM.Parser as P
import qualified Language.LLVM.AST as A
import qualified LLVM.General.AST as L

class ToDefintions a where
  toDefinitions :: a -> [L.Definition]
instance ToDefintions L.Definition where
  toDefinitions = (:[])
instance ToDefintions L.Global where
  toDefinitions = (:[]) . L.GlobalDefinition
instance ToDefintions a => ToDefintions [a] where
  toDefinitions xs = xs >>= toDefinitions

class ToBasicBlocks a where
  toBasicBlocks :: a -> [L.BasicBlock]
instance ToBasicBlocks L.BasicBlock where
  toBasicBlocks = (:[])
instance ToBasicBlocks a => ToBasicBlocks [a] where
  toBasicBlocks xs = xs >>= toBasicBlocks

antiVarE :: String -> ExpQ
antiVarE = either fail return . parseExp

qqDefinitionListE :: [A.Definition] -> Maybe (Q Exp)
qqDefinitionListE [] = Just [|[]|]
qqDefinitionListE (A.AntiDefinitionList v : defs) =
    Just [|toDefinitions $(antiVarE v) ++ $(qqE defs)|]
qqDefinitionListE (def : defs) =
    Just [|$(qqE def) : $(qqE defs)|]

qqDefinitionE :: A.Definition -> Maybe (Q Exp)
qqDefinitionE (A.GlobalDefinition v) =
    Just [|L.GlobalDefinition $(qqE v) :: L.Definition|]
qqDefinitionE (A.TypeDefinition n v) =
    Just [|L.TypeDefinition $(qqE n) $(qqE v) :: L.Definition|]
qqDefinitionE (A.MetadataNodeDefinition i vs) =
    Just [|L.MetadataNodeDefinition $(qqE i) $(qqE vs) :: L.Definition|]
qqDefinitionE (A.NamedMetadataDefinition i vs) =
    Just [|L.NamedMetadataDefinition $(qqE i) $(qqE vs) :: L.Definition|]
qqDefinitionE (A.ModuleInlineAssembly s) =
    Just [|L.ModuleInlineAssembly $(qqE s) :: L.Definition|]
qqDefinitionE a@(A.AntiDefinitionList _s) =
    error $ "Internal Error: unexpected antiquote " ++ show a

qqModuleE :: A.Module -> Maybe (Q Exp)
qqModuleE (A.Module n dl tt ds) = 
  Just [|L.Module $(qqE n) $(qqE dl) $(qqE tt) $(qqE ds) :: L.Module|]

qqGlobalE :: A.Global -> Maybe (Q Exp)
qqGlobalE (A.GlobalVariable x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB) =
  Just [|L.GlobalVariable $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                          $(qqE x6) $(qqE x7) $(qqE x8) $(qqE x9) $(qqE xA)
                          $(qqE xB)|]
qqGlobalE (A.GlobalAlias x1 x2 x3 x4 x5) =
  Just [|L.GlobalAlias $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqGlobalE (A.Function x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC) =
  Just [|L.Function $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                    $(qqE x6) $(qqE x7) $(qqE x8) $(qqE x9) $(qqE xA)
                    $(qqE xB) $(qqE xC)|]

qqParameterE :: A.Parameter -> Maybe (Q Exp)
qqParameterE (A.Parameter x1 x2 x3) =
  Just [|L.Parameter $(qqE x1) $(qqE x2) $(qqE x3)|]

qqBasicBlockListE :: [A.BasicBlock] -> Maybe (Q Exp)
qqBasicBlockListE [] = Just [|[]|]
qqBasicBlockListE (A.AntiBasicBlocks v : defs) =
    Just [|toBasicBlocks $(antiVarE v) ++ $(qqE defs)|]
qqBasicBlockListE (def : defs) =
    Just [|$(qqE def) : $(qqE defs)|]

qqBasicBlockE :: A.BasicBlock -> Maybe (Q Exp)
qqBasicBlockE (A.BasicBlock x1 x2 x3) =
  Just [|L.BasicBlock $(qqE x1) $(qqE x2) $(qqE x3)|]
qqBasicBlockE a@(A.AntiBasicBlocks _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqTerminatorE :: A.Terminator -> Maybe (Q Exp)
qqTerminatorE (A.Ret x1 x2) =
  Just [|L.Ret $(qqE x1) $(qqE x2)|]
qqTerminatorE (A.CondBr x1 x2 x3 x4) =
  Just [|L.CondBr $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqTerminatorE (A.Br x1 x2) =
  Just [|L.Br $(qqE x1) $(qqE x2)|]
qqTerminatorE (A.Switch x1 x2 x3 x4) =
  Just [|L.Switch $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqTerminatorE (A.IndirectBr x1 x2 x3) =
  Just [|L.IndirectBr $(qqE x1) $(qqE x2) $(qqE x3)|]
qqTerminatorE (A.Invoke x1 x2 x3 x4 x5 x6 x7 x8) =
  Just [|L.Invoke $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                  $(qqE x6) $(qqE x7) $(qqE x8)|]
qqTerminatorE (A.Resume x1 x2) =
  Just [|L.Resume $(qqE x1) $(qqE x2)|]
qqTerminatorE (A.Unreachable x1) =
  Just [|L.Unreachable $(qqE x1)|]

qqMemoryOrderingE :: A.MemoryOrdering -> Maybe (Q Exp)
qqMemoryOrderingE A.Unordered =
  Just [|L.Unordered|]
qqMemoryOrderingE A.Monotonic =
  Just [|L.Monotonic|]
qqMemoryOrderingE A.Acquire =
  Just [|L.Acquire|]
qqMemoryOrderingE A.Release =
  Just [|L.Release|]
qqMemoryOrderingE A.AcquireRelease =
  Just [|L.AcquireRelease|]
qqMemoryOrderingE A.SequentiallyConsistent =
  Just [|L.SequentiallyConsistent|]

qqAtomicityE :: A.Atomicity -> Maybe (Q Exp)
qqAtomicityE (A.Atomicity x1 x2) =
  Just [|L.Atomicity $(qqE x1) $(qqE x2)|]

qqLandingPadClauseE :: A.LandingPadClause -> Maybe (Q Exp)
qqLandingPadClauseE (A.Catch x1) =
  Just [|L.Catch $(qqE x1)|]
qqLandingPadClauseE (A.Filter x1) =
  Just [|L.Filter $(qqE x1)|]

qqInstructionE :: A.Instruction -> Maybe (Q Exp)
qqInstructionE (A.Add x1 x2 x3 x4 x5) =
  Just [|L.Add $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqInstructionE (A.FAdd x1 x2 x3) =
  Just [|L.FAdd $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Sub x1 x2 x3 x4 x5) =
  Just [|L.Sub $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqInstructionE (A.FSub x1 x2 x3) =
  Just [|L.FSub $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Mul x1 x2 x3 x4 x5) =
  Just [|L.Mul $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqInstructionE (A.FMul x1 x2 x3) =
  Just [|L.FMul $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.UDiv x1 x2 x3 x4) =
  Just [|L.UDiv $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.SDiv x1 x2 x3 x4) =
  Just [|L.SDiv $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.FDiv x1 x2 x3) =
  Just [|L.FDiv $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.URem x1 x2 x3) =
  Just [|L.URem $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.SRem x1 x2 x3) =
  Just [|L.SRem $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.FRem x1 x2 x3) =
  Just [|L.FRem $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Shl x1 x2 x3 x4 x5) =
  Just [|L.Shl $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqInstructionE (A.LShr x1 x2 x3 x4) =
  Just [|L.LShr $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.AShr x1 x2 x3 x4) =
  Just [|L.AShr $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.And x1 x2 x3) =
  Just [|L.And $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Or x1 x2 x3) =
  Just [|L.Or $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Xor x1 x2 x3) =
  Just [|L.Xor $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Alloca x1 x2 x3 x4) =
  Just [|L.Alloca $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.Load x1 x2 x3 x4 x5) =
  Just [|L.Load $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqInstructionE (A.Store x1 x2 x3 x4 x5 x6) =
  Just [|L.Store $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                 $(qqE x6)|]
qqInstructionE (A.GetElementPtr x1 x2 x3 x4) =
  Just [|L.GetElementPtr $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.Fence x1 x2) =
  Just [|L.Fence $(qqE x1) $(qqE x2)|]
qqInstructionE (A.CmpXchg x1 x2 x3 x4 x5 x6) =
  Just [|L.CmpXchg $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                  $(qqE x6)|]
qqInstructionE (A.AtomicRMW x1 x2 x3 x4 x5 x6) =
  Just [|L.AtomicRMW $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                     $(qqE x6)|]
qqInstructionE (A.Trunc x1 x2 x3) =
  Just [|L.Trunc $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.ZExt x1 x2 x3) =
  Just [|L.ZExt $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.SExt x1 x2 x3) =
  Just [|L.SExt $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.FPToUI x1 x2 x3) =
  Just [|L.FPToUI $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.FPToSI x1 x2 x3) =
  Just [|L.FPToSI $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.UIToFP x1 x2 x3) =
  Just [|L.UIToFP $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.SIToFP x1 x2 x3) =
  Just [|L.SIToFP $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.FPTrunc x1 x2 x3) =
  Just [|L.FPTrunc $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.FPExt x1 x2 x3) =
  Just [|L.FPExt $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.PtrToInt x1 x2 x3) =
  Just [|L.PtrToInt $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.IntToPtr x1 x2 x3) =
  Just [|L.IntToPtr $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.BitCast x1 x2 x3) =
  Just [|L.BitCast $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.AddrSpaceCast x1 x2 x3) =
  Just [|L.AddrSpaceCast $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.ICmp x1 x2 x3 x4) =
  Just [|L.ICmp $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.FCmp x1 x2 x3 x4) =
  Just [|L.FCmp $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.Phi x1 x2 x3) =
  Just [|L.Phi $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.Call x1 x2 x3 x4 x5 x6 x7) =
  Just [|L.Call $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                $(qqE x6) $(qqE x7)|]
qqInstructionE (A.Select x1 x2 x3 x4) =
  Just [|L.Select $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.VAArg x1 x2 x3) =
  Just [|L.VAArg $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.ExtractElement x1 x2 x3) =
  Just [|L.ExtractElement $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.InsertElement x1 x2 x3 x4) =
  Just [|L.InsertElement $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.ShuffleVector x1 x2 x3 x4) =
  Just [|L.ShuffleVector $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.ExtractValue x1 x2 x3) =
  Just [|L.ExtractValue $(qqE x1) $(qqE x2) $(qqE x3)|]
qqInstructionE (A.InsertValue x1 x2 x3 x4) =
  Just [|L.InsertValue $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4)|]
qqInstructionE (A.LandingPad x1 x2 x3 x4 x5) =
  Just [|L.LandingPad $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqInstructionE (A.AntiInstruction s) =
  Just $ antiVarE s

qqNamedE :: (Typeable a, Data a) => A.Named a -> Maybe (Q Exp)
qqNamedE ((A.:=) x1 x2) =
  Just [|(L.:=) $(qqE x1) $(qqE x2)|]
qqNamedE (A.Do x1) =
  Just [|L.Do $(qqE x1)|]

qqE :: Data a => a -> Q Exp
qqE x = dataToExpQ qqExp x

qqExp :: Typeable a => a -> Maybe (Q Exp)
qqExp = const Nothing `extQ` qqDefinitionE
                      `extQ` qqDefinitionListE
                      `extQ` qqModuleE
                      `extQ` qqGlobalE
                      `extQ` qqParameterE
                      `extQ` qqBasicBlockE
                      `extQ` qqBasicBlockListE
                      `extQ` qqTerminatorE
                      `extQ` qqMemoryOrderingE
                      `extQ` qqAtomicityE
                      `extQ` qqLandingPadClauseE
                      `extQ` qqInstructionE
                      `extQ` (qqNamedE :: A.Named A.Instruction -> Maybe (Q Exp))
                      `extQ` (qqNamedE :: A.Named A.Terminator -> Maybe (Q Exp))

parse :: [A.Extensions]
      -> P.P a
      -> String
      -> Q a
parse exts p s = do
    loc <- location
    case P.parse (A.Antiquotation : exts) p (B.pack s) (locToPos loc) of
      Left err -> fail (show err)
      Right x  -> return x
  where
    locToPos :: Language.Haskell.TH.Loc -> Pos
    locToPos loc = Pos (loc_filename loc)
                       ((fst . loc_start) loc)
                       ((snd . loc_start) loc)
                       0

quasiquote :: Data a
           => [A.Extensions]
           -> P.P a
           -> QuasiQuoter
quasiquote exts p =
    QuasiQuoter { quoteExp  = parse exts p >=> dataToExpQ qqExp
                , quotePat  = fail "LLVM pattern quasiquoter undefined"
                , quoteType = fail "LLVM type quasiquoter undefined"
                , quoteDec  = fail "LLVM declaration quasiquoter undefined"
                }