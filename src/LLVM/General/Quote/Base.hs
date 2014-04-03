{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -Werror #-}

module LLVM.General.Quote.Base (
    ToDefintions(..),
    quasiquote,
    parse
  ) where

import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as B
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Word
import Data.Loc
import Data.Typeable (Typeable)
import Language.Haskell.Meta (parseExp, parsePat)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToExpQ,
                                  dataToPatQ)

import qualified LLVM.General.Quote.Parser as P
import qualified LLVM.General.Quote.AST as A
import qualified LLVM.General.AST.IntegerPredicate as AI
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Constant as L
  (Constant(Int, Float, Null, Struct, Array, Vector, Undef, BlockAddress, GlobalReference))
import qualified LLVM.General.AST.Float as L
import qualified LLVM.General.AST.InlineAssembly as L
import qualified LLVM.General.AST.DataLayout as L
import qualified LLVM.General.AST.AddrSpace as A

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

class ToDefintion a where
  toDefinition :: a -> L.Definition
instance ToDefintion L.Definition where
  toDefinition = id
instance ToDefintion L.Global where
  toDefinition = L.GlobalDefinition

class ToDefintions a where
  toDefinitions :: a -> [L.Definition]
instance ToDefintion a => ToDefintions [a] where
  toDefinitions = map toDefinition

class ToConstant a where
  toConstant :: a -> L.Constant
instance ToConstant Word8 where
  toConstant n = L.Int 8 (toInteger n)
instance ToConstant Word16 where
  toConstant n = L.Int 16 (toInteger n)
instance ToConstant Word32 where
  toConstant n = L.Int 32 (toInteger n)
instance ToConstant Word64 where
  toConstant n = L.Int 64 (toInteger n)
instance ToConstant Float where
  toConstant n = L.Float (L.Single n)
instance ToConstant Double where
  toConstant n = L.Float (L.Double n)

class ToName a where
  toName :: a -> L.Name
instance ToName L.Name where
  toName = id
instance ToName String where
  toName = L.Name
instance ToName Word where
  toName = L.UnName

class ToTargetTriple a where
  toTargetTriple :: a -> Maybe String
instance ToTargetTriple String where
  toTargetTriple = Just
instance ToTargetTriple (Maybe String) where
  toTargetTriple = id

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
qqDefinitionE a@(A.AntiDefinition s) =
    Just $ antiVarE s
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

qqParameterListE :: [A.Parameter] -> Maybe (Q Exp)
qqParameterListE [] = Just [|[]|]
qqParameterListE (A.AntiParameterList v : defs) =
    Just [|$(antiVarE v) ++ $(qqE defs)|]
qqParameterListE (def : defs) =
    Just [|$(qqE def) : $(qqE defs)|]

qqParameterE :: A.Parameter -> Maybe (Q Exp)
qqParameterE (A.Parameter x1 x2 x3) =
  Just [|L.Parameter $(qqE x1) $(qqE x2) $(qqE x3)|]
qqParameterE (A.AntiParameter s) =
  Just $ antiVarE s
qqParameterE a@(A.AntiParameterList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqBasicBlockListE :: [A.BasicBlock] -> Maybe (Q Exp)
qqBasicBlockListE [] = Just [|[]|]
qqBasicBlockListE (for@A.ForLoop{} : defs) =
  Just [|$(qqE $ transform for) ++ $(qqE defs)|]
qqBasicBlockListE (A.AntiBasicBlockList v : defs) =
  Just [|$(antiVarE v) ++ $(qqE defs)|]
qqBasicBlockListE (def : defs) =
  Just [|$(qqE def) : $(qqE defs)|]

transform :: A.BasicBlock -> [A.BasicBlock]
transform bb@A.BasicBlock{} = [bb]
transform (A.ForLoop label iterType iterName from to elementType element elementName body next) =
    let labelString = case label of
                        A.Name s -> s
                        A.UnName n -> "num"++show n
                        A.AntiName s -> error $ "Error: antiquotation for names not legal in for-header " ++ s
        cond = A.Name $ labelString ++ ".cond"
        iterNameNew = A.Name $ case iterName of
                        A.Name s -> s ++ ".new"
                        A.UnName n -> "num"++show n++".new"
                        A.AntiName s -> error $ "Error: antiquotation for names not legal in for-header " ++ s
        iterBits = case iterType of
                     A.IntegerType n -> n
                     t -> error $ "Internal Error: unexpected type " ++ show t
        iter = (A.LocalReference iterName)
        --labels = map A.label body
        preInstrs = 
          [ iterName A.:= A.Phi iterType (map (\(_,l) -> (A.LocalReference iterNameNew,l)) returns ++ map (\(_,s) -> (from,s)) element) []
          , elementName A.:= A.Phi elementType (returns ++ element) []
          , cond A.:= A.ICmp AI.ULE iter to []
          , iterNameNew A.:= A.Add True True iter (A.ConstantOperand $ A.Int iterBits 1) []
          ]
        body' = body >>= transform
        bodyLabel = A.label $ head body'
        returns = body' >>= maybeToList . ret
        pre  = case next of
                 Just next' -> A.BasicBlock label preInstrs (A.Do $ A.CondBr (A.LocalReference cond) bodyLabel next' [])
                 Nothing    -> A.BasicBlock label preInstrs (A.Do $ A.Ret (Just $ A.LocalReference elementName) [])
        main = map (replaceRet label) body'
    in (pre:main)
transform A.AntiBasicBlock{}
  = error $ "Error: antiquotation of BasicBlocks not allowed inside a loop"
transform A.AntiBasicBlockList{}
  = error $ "Error: antiquotation of BasicBlocks not allowed inside a loop"

ret :: A.BasicBlock -> Maybe (A.Operand, A.Name)
ret (A.BasicBlock l _ t') = do
  let t = case t' of
            _ A.:= t'' -> t''
            A.Do t''   -> t''
  A.Ret (Just x) _ <- return t
  return (x,l)
ret x = error $ "Internal Error: only plain BasicBlocks should arrive at function ret. got: " ++ show x

replaceRet :: A.Name -> A.BasicBlock -> A.BasicBlock
replaceRet label bb@A.BasicBlock{} =
  case A.terminator bb of
    n A.:= A.Ret _ md -> bb{ A.terminator = n A.:= A.Br label md }
    A.Do (A.Ret _ md) -> bb{ A.terminator = A.Do (A.Br label md) }
    _                 -> bb
replaceRet _ x = error $ "Internal Error: only plain BasicBlocks should arrive at function replaceRet. got: " ++ show x

qqBasicBlockE :: A.BasicBlock -> Maybe (Q Exp)
qqBasicBlockE (A.BasicBlock x1 x2 x3) =
  Just [|L.BasicBlock $(qqE x1) $(qqE x2) $(qqE x3)|]
qqBasicBlockE (A.AntiBasicBlock s) =
  Just $ antiVarE s
qqBasicBlockE a@A.ForLoop{} =
  error $ "Internal Error: unexpected loop " ++ show a
qqBasicBlockE a@(A.AntiBasicBlockList _s) =
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

qqMetadataNodeIDE :: A.MetadataNodeID -> Maybe (Q Exp)
qqMetadataNodeIDE (A.MetadataNodeID x1) =
  Just [|L.MetadataNodeID $(qqE x1)|]

qqMetadataNodeE :: A.MetadataNode -> Maybe (Q Exp)
qqMetadataNodeE (A.MetadataNode x1) =
  Just [|L.MetadataNode $(qqE x1)|]
qqMetadataNodeE (A.MetadataNodeReference x1) =
  Just [|L.MetadataNodeReference $(qqE x1)|]

qqOperandE :: A.Operand -> Maybe (Q Exp)
qqOperandE (A.LocalReference x1) =
  Just [|L.LocalReference $(qqE x1)|]
qqOperandE (A.ConstantOperand x1) =
  Just [|L.ConstantOperand $(qqE x1)|]
qqOperandE (A.MetadataStringOperand x1) =
  Just [|L.MetadataStringOperand $(qqE x1)|]
qqOperandE (A.MetadataNodeOperand x1) =
  Just [|L.MetadataNodeOperand $(qqE x1)|]
qqOperandE (A.AntiOperand s) =
  Just $ antiVarE s

qqConstantE :: A.Constant -> Maybe (Q Exp)
qqConstantE (A.Int x1 x2) =
  Just [|L.Int $(qqE x1) $(qqE x2)|]
qqConstantE (A.Float x1) =
  Just [|L.Float $(qqE x1)|]
qqConstantE (A.Null x1) =
  Just [|L.Null $(qqE x1)|]
qqConstantE (A.Struct x1 x2 x3) =
  Just [|L.Struct $(qqE x1) $(qqE x2) $(qqE x3)|]
qqConstantE (A.Array x1 x2) =
  Just [|L.Array $(qqE x1) $(qqE x2)|]
qqConstantE (A.Vector x1) =
  Just [|L.Vector $(qqE x1)|]
qqConstantE (A.Undef x1) =
  Just [|L.Undef $(qqE x1)|]
qqConstantE (A.BlockAddress x1 x2) =
  Just [|L.BlockAddress $(qqE x1) $(qqE x2)|]
qqConstantE (A.GlobalReference x1) =
  Just [|L.GlobalReference $(qqE x1)|]
qqConstantE (A.AntiConstant s) =
  Just [|toConstant $(antiVarE s)|]

qqNameE :: A.Name -> Maybe (Q Exp)
qqNameE (A.Name x1) =
  Just [|L.Name $(qqE x1)|]
qqNameE (A.UnName x1) =
  Just [|L.UnName $(qqE x1)|]
qqNameE (A.AntiName s) =
  Just [|toName $(antiVarE s)|]

qqFloatingPointFormatE :: A.FloatingPointFormat -> Maybe (Q Exp)
qqFloatingPointFormatE A.IEEE =
  Just [|L.IEEE|]
qqFloatingPointFormatE A.DoubleExtended =
  Just [|L.DoubleExtended|]
qqFloatingPointFormatE A.PairOfFloats =
  Just [|L.PairOfFloats|]

qqTypeE :: A.Type -> Maybe (Q Exp)
qqTypeE A.VoidType =
  Just [|L.VoidType|]
qqTypeE (A.IntegerType x1) =
  Just [|L.IntegerType $(qqE x1)|]
qqTypeE (A.PointerType x1 x2) =
  Just [|L.PointerType $(qqE x1) $(qqE x2)|]
qqTypeE (A.FloatingPointType x1 x2) =
  Just [|L.FloatingPointType $(qqE x1) $(qqE x2)|]
qqTypeE (A.FunctionType x1 x2 x3) =
  Just [|L.FunctionType $(qqE x1) $(qqE x2) $(qqE x3)|]
qqTypeE (A.VectorType x1 x2) =
  Just [|L.VectorType $(qqE x1) $(qqE x2)|]
qqTypeE (A.StructureType x1 x2) =
  Just [|L.StructureType $(qqE x1) $(qqE x2)|]
qqTypeE (A.ArrayType x1 x2) =
  Just [|L.ArrayType $(qqE x1) $(qqE x2)|]
qqTypeE (A.NamedTypeReference x1) =
  Just [|L.NamedTypeReference $(qqE x1)|]
qqTypeE A.MetadataType =
  Just [|L.MetadataType|]
qqTypeE (A.AntiType s) =
  Just $ antiVarE s

qqDialectE :: A.Dialect -> Maybe (Q Exp)
qqDialectE A.ATTDialect =
  Just [|L.ATTDialect|]
qqDialectE A.IntelDialect =
  Just [|L.IntelDialect|]

qqInlineAssemblyE :: A.InlineAssembly -> Maybe (Q Exp)
qqInlineAssemblyE (A.InlineAssembly x1 x2 x3 x4 x5 x6) =
  Just [|L.InlineAssembly $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)
                          $(qqE x6)|]

qqMapE :: (Data a, Data b) => M.Map a b -> Maybe (Q Exp)
qqMapE m =
  Just [|M.fromList $(qqE (M.toList m))|]

qqSetE :: Data a => S.Set a -> Maybe (Q Exp)
qqSetE m =
  Just [|S.fromList $(qqE (S.toList m))|]

qqEndiannessE :: A.Endianness -> Maybe (Q Exp)
qqEndiannessE A.LittleEndian =
  Just [|L.LittleEndian|]
qqEndiannessE A.BigEndian =
  Just [|L.BigEndian|]

qqAlignmentInfoE :: A.AlignmentInfo -> Maybe (Q Exp)
qqAlignmentInfoE (A.AlignmentInfo x1 x2) =
  Just [|L.AlignmentInfo $(qqE x1) $(qqE x2)|]

qqAlignTypeE :: A.AlignType -> Maybe (Q Exp)
qqAlignTypeE A.IntegerAlign =
  Just [|L.IntegerAlign|]
qqAlignTypeE A.VectorAlign =
  Just [|L.VectorAlign|]
qqAlignTypeE A.FloatAlign =
  Just [|L.FloatAlign|]
qqAlignTypeE A.AggregateAlign =
  Just [|L.AggregateAlign|]
qqAlignTypeE A.StackAlign =
  Just [|L.StackAlign|]

qqDataLayoutE :: A.DataLayout -> Maybe (Q Exp)
qqDataLayoutE (A.DataLayout x1 x2 x3 x4 x5) =
  Just [|L.DataLayout $(qqE x1) $(qqE x2) $(qqE x3) $(qqE x4) $(qqE x5)|]
qqDataLayoutE (A.AntiDataLayout s) =
  Just $ antiVarE s

qqTargetTripleE :: A.TargetTriple -> Maybe (Q Exp)
qqTargetTripleE A.NoTargetTriple =
  Just [|Nothing|]
qqTargetTripleE (A.TargetTriple v) =
  Just [|Just $(qqE v)|]
qqTargetTripleE (A.AntiTargetTriple v) =
  Just [|toTargetTriple $(antiVarE v)|]


qqE :: Data a => a -> Q Exp
qqE x = dataToExpQ qqExp x

qqExp :: Typeable a => a -> Maybe (Q Exp)
qqExp = const Nothing `extQ` qqDefinitionE
                      `extQ` qqDefinitionListE
                      `extQ` qqModuleE
                      `extQ` qqGlobalE
                      `extQ` qqParameterListE
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
                      `extQ` qqMetadataNodeIDE
                      `extQ` qqMetadataNodeE
                      `extQ` qqOperandE
                      `extQ` qqConstantE
                      `extQ` qqNameE
                      `extQ` qqFloatingPointFormatE
                      `extQ` qqTypeE
                      `extQ` qqDialectE
                      `extQ` qqInlineAssemblyE
                      `extQ` (qqMapE :: M.Map A.AddrSpace (Word32, A.AlignmentInfo) -> Maybe (Q Exp))
                      `extQ` (qqMapE :: M.Map (A.AlignType, Word32) A.AlignmentInfo -> Maybe (Q Exp))
                      `extQ` (qqSetE :: S.Set Word32 -> Maybe (Q Exp))
                      `extQ` qqEndiannessE
                      `extQ` qqAlignmentInfoE
                      `extQ` qqAlignTypeE
                      `extQ` qqDataLayoutE
                      `extQ` qqTargetTripleE


antiVarP :: String -> PatQ
antiVarP = either fail return . parsePat

qqDefinitionListP :: [A.Definition] -> Maybe (Q Pat)
qqDefinitionListP [] = Just [p|[]|]
qqDefinitionListP [A.AntiDefinitionList v] =
    Just $ antiVarP v
qqDefinitionListP (A.AntiDefinitionList _ : _ : _) =
    error "Antiquoted list of definitions must be last item in quoted list"
qqDefinitionListP (def : defs) =
    Just [p|$(qqP def) : $(qqP defs)|]

qqDefinitionP :: A.Definition -> Maybe (Q Pat)
qqDefinitionP (A.GlobalDefinition v) =
    Just [p|L.GlobalDefinition $(qqP v)|]
qqDefinitionP (A.TypeDefinition n v) =
    Just [p|L.TypeDefinition $(qqP n) $(qqP v)|]
qqDefinitionP (A.MetadataNodeDefinition i vs) =
    Just [p|L.MetadataNodeDefinition $(qqP i) $(qqP vs)|]
qqDefinitionP (A.NamedMetadataDefinition i vs) =
    Just [p|L.NamedMetadataDefinition $(qqP i) $(qqP vs)|]
qqDefinitionP (A.ModuleInlineAssembly s) =
    Just [p|L.ModuleInlineAssembly $(qqP s)|]
qqDefinitionP (A.AntiDefinition s) =
    Just $ antiVarP s
qqDefinitionP a@(A.AntiDefinitionList _s) =
    error $ "Internal Error: unexpected antiquote " ++ show a

qqModuleP :: A.Module -> Maybe (Q Pat)
qqModuleP (A.Module n dl tt ds) = 
  Just [p|L.Module $(qqP n) $(qqP dl) $(qqP tt) $(qqP ds)|]

qqGlobalP :: A.Global -> Maybe (Q Pat)
qqGlobalP (A.GlobalVariable x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB) =
  Just [p|L.GlobalVariable $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                          $(qqP x6) $(qqP x7) $(qqP x8) $(qqP x9) $(qqP xA)
                          $(qqP xB)|]
qqGlobalP (A.GlobalAlias x1 x2 x3 x4 x5) =
  Just [p|L.GlobalAlias $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqGlobalP (A.Function x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC) =
  Just [p|L.Function $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                    $(qqP x6) $(qqP x7) $(qqP x8) $(qqP x9) $(qqP xA)
                    $(qqP xB) $(qqP xC)|]

qqParameterListP :: [A.Parameter] -> Maybe (Q Pat)
qqParameterListP [] = Just [p|[]|]
qqParameterListP [A.AntiParameterList v] =
    Just $ antiVarP v
qqParameterListP (A.AntiParameterList v : _) =
    error "Antiquoted list of Parameters must be last item in quoted list"
qqParameterListP (def : defs) =
    Just [p|$(qqP def) : $(qqP defs)|]

qqParameterP :: A.Parameter -> Maybe (Q Pat)
qqParameterP (A.Parameter x1 x2 x3) =
  Just [p|L.Parameter $(qqP x1) $(qqP x2) $(qqP x3)|]
qqParameterP (A.AntiParameter s) =
  Just $ antiVarP s
qqParameterP a@(A.AntiParameterList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqBasicBlockListP :: [A.BasicBlock] -> Maybe (Q Pat)
qqBasicBlockListP [] = Just [p|[]|]
qqBasicBlockListP [A.AntiBasicBlockList v] =
    Just $ antiVarP v
qqBasicBlockListP (A.AntiBasicBlockList v : defs) =
    error "Antiquoted list of BasicBlocks must be last item in quoted list"
qqBasicBlockListP (def : defs) =
    Just [p|$(qqP def) : $(qqP defs)|]

qqBasicBlockP :: A.BasicBlock -> Maybe (Q Pat)
qqBasicBlockP (A.BasicBlock x1 x2 x3) =
  Just [p|L.BasicBlock $(qqP x1) $(qqP x2) $(qqP x3)|]
qqBasicBlockP (A.AntiBasicBlock s) =
  Just $ antiVarP s
qqBasicBlockP a@A.ForLoop{} =
  error $ "Error: for-loop not allowed in pattern quote " ++ show a
qqBasicBlockP a@(A.AntiBasicBlockList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqTerminatorP :: A.Terminator -> Maybe (Q Pat)
qqTerminatorP (A.Ret x1 x2) =
  Just [p|L.Ret $(qqP x1) $(qqP x2)|]
qqTerminatorP (A.CondBr x1 x2 x3 x4) =
  Just [p|L.CondBr $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqTerminatorP (A.Br x1 x2) =
  Just [p|L.Br $(qqP x1) $(qqP x2)|]
qqTerminatorP (A.Switch x1 x2 x3 x4) =
  Just [p|L.Switch $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqTerminatorP (A.IndirectBr x1 x2 x3) =
  Just [p|L.IndirectBr $(qqP x1) $(qqP x2) $(qqP x3)|]
qqTerminatorP (A.Invoke x1 x2 x3 x4 x5 x6 x7 x8) =
  Just [p|L.Invoke $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                  $(qqP x6) $(qqP x7) $(qqP x8)|]
qqTerminatorP (A.Resume x1 x2) =
  Just [p|L.Resume $(qqP x1) $(qqP x2)|]
qqTerminatorP (A.Unreachable x1) =
  Just [p|L.Unreachable $(qqP x1)|]

qqMemoryOrderingP :: A.MemoryOrdering -> Maybe (Q Pat)
qqMemoryOrderingP A.Unordered =
  Just [p|L.Unordered|]
qqMemoryOrderingP A.Monotonic =
  Just [p|L.Monotonic|]
qqMemoryOrderingP A.Acquire =
  Just [p|L.Acquire|]
qqMemoryOrderingP A.Release =
  Just [p|L.Release|]
qqMemoryOrderingP A.AcquireRelease =
  Just [p|L.AcquireRelease|]
qqMemoryOrderingP A.SequentiallyConsistent =
  Just [p|L.SequentiallyConsistent|]

qqAtomicityP :: A.Atomicity -> Maybe (Q Pat)
qqAtomicityP (A.Atomicity x1 x2) =
  Just [p|L.Atomicity $(qqP x1) $(qqP x2)|]

qqLandingPadClauseP :: A.LandingPadClause -> Maybe (Q Pat)
qqLandingPadClauseP (A.Catch x1) =
  Just [p|L.Catch $(qqP x1)|]
qqLandingPadClauseP (A.Filter x1) =
  Just [p|L.Filter $(qqP x1)|]

qqInstructionP :: A.Instruction -> Maybe (Q Pat)
qqInstructionP (A.Add x1 x2 x3 x4 x5) =
  Just [p|L.Add $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqInstructionP (A.FAdd x1 x2 x3) =
  Just [p|L.FAdd $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Sub x1 x2 x3 x4 x5) =
  Just [p|L.Sub $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqInstructionP (A.FSub x1 x2 x3) =
  Just [p|L.FSub $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Mul x1 x2 x3 x4 x5) =
  Just [p|L.Mul $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqInstructionP (A.FMul x1 x2 x3) =
  Just [p|L.FMul $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.UDiv x1 x2 x3 x4) =
  Just [p|L.UDiv $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.SDiv x1 x2 x3 x4) =
  Just [p|L.SDiv $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.FDiv x1 x2 x3) =
  Just [p|L.FDiv $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.URem x1 x2 x3) =
  Just [p|L.URem $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.SRem x1 x2 x3) =
  Just [p|L.SRem $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.FRem x1 x2 x3) =
  Just [p|L.FRem $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Shl x1 x2 x3 x4 x5) =
  Just [p|L.Shl $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqInstructionP (A.LShr x1 x2 x3 x4) =
  Just [p|L.LShr $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.AShr x1 x2 x3 x4) =
  Just [p|L.AShr $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.And x1 x2 x3) =
  Just [p|L.And $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Or x1 x2 x3) =
  Just [p|L.Or $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Xor x1 x2 x3) =
  Just [p|L.Xor $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Alloca x1 x2 x3 x4) =
  Just [p|L.Alloca $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.Load x1 x2 x3 x4 x5) =
  Just [p|L.Load $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqInstructionP (A.Store x1 x2 x3 x4 x5 x6) =
  Just [p|L.Store $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                 $(qqP x6)|]
qqInstructionP (A.GetElementPtr x1 x2 x3 x4) =
  Just [p|L.GetElementPtr $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.Fence x1 x2) =
  Just [p|L.Fence $(qqP x1) $(qqP x2)|]
qqInstructionP (A.CmpXchg x1 x2 x3 x4 x5 x6) =
  Just [p|L.CmpXchg $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                  $(qqP x6)|]
qqInstructionP (A.AtomicRMW x1 x2 x3 x4 x5 x6) =
  Just [p|L.AtomicRMW $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                     $(qqP x6)|]
qqInstructionP (A.Trunc x1 x2 x3) =
  Just [p|L.Trunc $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.ZExt x1 x2 x3) =
  Just [p|L.ZExt $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.SExt x1 x2 x3) =
  Just [p|L.SExt $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.FPToUI x1 x2 x3) =
  Just [p|L.FPToUI $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.FPToSI x1 x2 x3) =
  Just [p|L.FPToSI $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.UIToFP x1 x2 x3) =
  Just [p|L.UIToFP $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.SIToFP x1 x2 x3) =
  Just [p|L.SIToFP $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.FPTrunc x1 x2 x3) =
  Just [p|L.FPTrunc $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.FPExt x1 x2 x3) =
  Just [p|L.FPExt $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.PtrToInt x1 x2 x3) =
  Just [p|L.PtrToInt $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.IntToPtr x1 x2 x3) =
  Just [p|L.IntToPtr $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.BitCast x1 x2 x3) =
  Just [p|L.BitCast $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.AddrSpaceCast x1 x2 x3) =
  Just [p|L.AddrSpaceCast $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.ICmp x1 x2 x3 x4) =
  Just [p|L.ICmp $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.FCmp x1 x2 x3 x4) =
  Just [p|L.FCmp $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.Phi x1 x2 x3) =
  Just [p|L.Phi $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.Call x1 x2 x3 x4 x5 x6 x7) =
  Just [p|L.Call $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                $(qqP x6) $(qqP x7)|]
qqInstructionP (A.Select x1 x2 x3 x4) =
  Just [p|L.Select $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.VAArg x1 x2 x3) =
  Just [p|L.VAArg $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.ExtractElement x1 x2 x3) =
  Just [p|L.ExtractElement $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.InsertElement x1 x2 x3 x4) =
  Just [p|L.InsertElement $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.ShuffleVector x1 x2 x3 x4) =
  Just [p|L.ShuffleVector $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.ExtractValue x1 x2 x3) =
  Just [p|L.ExtractValue $(qqP x1) $(qqP x2) $(qqP x3)|]
qqInstructionP (A.InsertValue x1 x2 x3 x4) =
  Just [p|L.InsertValue $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4)|]
qqInstructionP (A.LandingPad x1 x2 x3 x4 x5) =
  Just [p|L.LandingPad $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqInstructionP (A.AntiInstruction s) =
  Just $ antiVarP s

qqNamedP :: (Typeable a, Data a) => A.Named a -> Maybe (Q Pat)
qqNamedP ((A.:=) x1 x2) =
  Just [p|(L.:=) $(qqP x1) $(qqP x2)|]
qqNamedP (A.Do x1) =
  Just [p|L.Do $(qqP x1)|]

qqMetadataNodeIDP :: A.MetadataNodeID -> Maybe (Q Pat)
qqMetadataNodeIDP (A.MetadataNodeID x1) =
  Just [p|L.MetadataNodeID $(qqP x1)|]

qqMetadataNodeP :: A.MetadataNode -> Maybe (Q Pat)
qqMetadataNodeP (A.MetadataNode x1) =
  Just [p|L.MetadataNode $(qqP x1)|]
qqMetadataNodeP (A.MetadataNodeReference x1) =
  Just [p|L.MetadataNodeReference $(qqP x1)|]

qqOperandP :: A.Operand -> Maybe (Q Pat)
qqOperandP (A.LocalReference x1) =
  Just [p|L.LocalReference $(qqP x1)|]
qqOperandP (A.ConstantOperand x1) =
  Just [p|L.ConstantOperand $(qqP x1)|]
qqOperandP (A.MetadataStringOperand x1) =
  Just [p|L.MetadataStringOperand $(qqP x1)|]
qqOperandP (A.MetadataNodeOperand x1) =
  Just [p|L.MetadataNodeOperand $(qqP x1)|]
qqOperandP (A.AntiOperand s) =
  Just $ antiVarP s

qqConstantP :: A.Constant -> Maybe (Q Pat)
qqConstantP (A.Int x1 x2) =
  Just [p|L.Int $(qqP x1) $(qqP x2)|]
qqConstantP (A.Float x1) =
  Just [p|L.Float $(qqP x1)|]
qqConstantP (A.Null x1) =
  Just [p|L.Null $(qqP x1)|]
qqConstantP (A.Struct x1 x2 x3) =
  Just [p|L.Struct $(qqP x1) $(qqP x2) $(qqP x3)|]
qqConstantP (A.Array x1 x2) =
  Just [p|L.Array $(qqP x1) $(qqP x2)|]
qqConstantP (A.Vector x1) =
  Just [p|L.Vector $(qqP x1)|]
qqConstantP (A.Undef x1) =
  Just [p|L.Undef $(qqP x1)|]
qqConstantP (A.BlockAddress x1 x2) =
  Just [p|L.BlockAddress $(qqP x1) $(qqP x2)|]
qqConstantP (A.GlobalReference x1) =
  Just [p|L.GlobalReference $(qqP x1)|]
qqConstantP (A.AntiConstant s) =
  Just $ antiVarP s

qqNameP :: A.Name -> Maybe (Q Pat)
qqNameP (A.Name x1) =
  Just [p|L.Name $(qqP x1)|]
qqNameP (A.UnName x1) =
  Just [p|L.UnName $(qqP x1)|]
qqNameP (A.AntiName s) =
  Just $ antiVarP s

qqFloatingPointFormatP :: A.FloatingPointFormat -> Maybe (Q Pat)
qqFloatingPointFormatP A.IEEE =
  Just [p|L.IEEE|]
qqFloatingPointFormatP A.DoubleExtended =
  Just [p|L.DoubleExtended|]
qqFloatingPointFormatP A.PairOfFloats =
  Just [p|L.PairOfFloats|]

qqTypeP :: A.Type -> Maybe (Q Pat)
qqTypeP A.VoidType =
  Just [p|L.VoidType|]
qqTypeP (A.IntegerType x1) =
  Just [p|L.IntegerType $(qqP x1)|]
qqTypeP (A.PointerType x1 x2) =
  Just [p|L.PointerType $(qqP x1) $(qqP x2)|]
qqTypeP (A.FloatingPointType x1 x2) =
  Just [p|L.FloatingPointType $(qqP x1) $(qqP x2)|]
qqTypeP (A.FunctionType x1 x2 x3) =
  Just [p|L.FunctionType $(qqP x1) $(qqP x2) $(qqP x3)|]
qqTypeP (A.VectorType x1 x2) =
  Just [p|L.VectorType $(qqP x1) $(qqP x2)|]
qqTypeP (A.StructureType x1 x2) =
  Just [p|L.StructureType $(qqP x1) $(qqP x2)|]
qqTypeP (A.ArrayType x1 x2) =
  Just [p|L.ArrayType $(qqP x1) $(qqP x2)|]
qqTypeP (A.NamedTypeReference x1) =
  Just [p|L.NamedTypeReference $(qqP x1)|]
qqTypeP A.MetadataType =
  Just [p|L.MetadataType|]
qqTypeP (A.AntiType s) =
  Just $ antiVarP s

qqDialectP :: A.Dialect -> Maybe (Q Pat)
qqDialectP A.ATTDialect =
  Just [p|L.ATTDialect|]
qqDialectP A.IntelDialect =
  Just [p|L.IntelDialect|]

qqInlineAssemblyP :: A.InlineAssembly -> Maybe (Q Pat)
qqInlineAssemblyP (A.InlineAssembly x1 x2 x3 x4 x5 x6) =
  Just [p|L.InlineAssembly $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)
                          $(qqP x6)|]

qqEndiannessP :: A.Endianness -> Maybe (Q Pat)
qqEndiannessP A.LittleEndian =
  Just [p|L.LittleEndian|]
qqEndiannessP A.BigEndian =
  Just [p|L.BigEndian|]

qqAlignmentInfoP :: A.AlignmentInfo -> Maybe (Q Pat)
qqAlignmentInfoP (A.AlignmentInfo x1 x2) =
  Just [p|L.AlignmentInfo $(qqP x1) $(qqP x2)|]

qqAlignTypeP :: A.AlignType -> Maybe (Q Pat)
qqAlignTypeP A.IntegerAlign =
  Just [p|L.IntegerAlign|]
qqAlignTypeP A.VectorAlign =
  Just [p|L.VectorAlign|]
qqAlignTypeP A.FloatAlign =
  Just [p|L.FloatAlign|]
qqAlignTypeP A.AggregateAlign =
  Just [p|L.AggregateAlign|]
qqAlignTypeP A.StackAlign =
  Just [p|L.StackAlign|]

qqDataLayoutP :: A.DataLayout -> Maybe (Q Pat)
qqDataLayoutP (A.DataLayout x1 x2 x3 x4 x5) =
  Just [p|L.DataLayout $(qqP x1) $(qqP x2) $(qqP x3) $(qqP x4) $(qqP x5)|]
qqDataLayoutP (A.AntiDataLayout s) =
  Just $ antiVarP s

qqTargetTripleP :: A.TargetTriple -> Maybe (Q Pat)
qqTargetTripleP A.NoTargetTriple =
  Just [p|Nothing|]
qqTargetTripleP (A.TargetTriple v) =
  Just [p|Just $(qqP v)|]
qqTargetTripleP (A.AntiTargetTriple v) =
  Just $ antiVarP v

qqP :: Data a => a -> Q Pat
qqP x = dataToPatQ qqPat x

qqPat :: Typeable a => a -> Maybe (Q Pat)
qqPat = const Nothing `extQ` qqDefinitionP
                      `extQ` qqDefinitionListP
                      `extQ` qqModuleP
                      `extQ` qqGlobalP
                      `extQ` qqParameterListP
                      `extQ` qqParameterP
                      `extQ` qqBasicBlockP
                      `extQ` qqBasicBlockListP
                      `extQ` qqTerminatorP
                      `extQ` qqMemoryOrderingP
                      `extQ` qqAtomicityP
                      `extQ` qqLandingPadClauseP
                      `extQ` qqInstructionP
                      `extQ` (qqNamedP :: A.Named A.Instruction -> Maybe (Q Pat))
                      `extQ` (qqNamedP :: A.Named A.Terminator -> Maybe (Q Pat))
                      `extQ` qqMetadataNodeIDP
                      `extQ` qqMetadataNodeP
                      `extQ` qqOperandP
                      `extQ` qqConstantP
                      `extQ` qqNameP
                      `extQ` qqFloatingPointFormatP
                      `extQ` qqTypeP
                      `extQ` qqDialectP
                      `extQ` qqInlineAssemblyP
                      `extQ` qqEndiannessP
                      `extQ` qqAlignmentInfoP
                      `extQ` qqAlignTypeP
                      `extQ` qqDataLayoutP
                      `extQ` qqTargetTripleP

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
    QuasiQuoter { quoteExp  = parse exts p >=> qqE
                , quotePat  = parse exts p >=> qqP
                , quoteType = fail "LLVM type quasiquoter undefined"
                , quoteDec  = fail "LLVM declaration quasiquoter undefined"
                }
