{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LLVM.General.Quote.Base (
    ToDefintions(..),
    quasiquote,
    TQuasiQuoter(..),
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
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToPatQ)

import qualified LLVM.General.Quote.Parser as P
import qualified LLVM.General.Quote.AST as A
import qualified LLVM.General.AST.IntegerPredicate as LI
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Constant as L
  (Constant(Int, Float, Null, Struct, Array, Vector, Undef, BlockAddress, GlobalReference))
import qualified LLVM.General.AST.Float as L
import qualified LLVM.General.AST.InlineAssembly as L
import qualified LLVM.General.AST.DataLayout as L
import qualified LLVM.General.AST.AddrSpace as L
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as L
import qualified LLVM.General.AST.CallingConvention as L
import qualified LLVM.General.AST.Attribute as L
import qualified LLVM.General.AST.RMWOperation as LR
import qualified LLVM.General.AST.FloatingPointPredicate as LF

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

antiVarE :: String -> TExpQ a
antiVarE = unsafeTExpCoerce . either fail return . parseExp

class QQExp a b where
  qqExp :: a -> TExpQ b

--instance (Lift a) => QQExp a a where
--  qqExp x = [||x||]
instance QQExp String String where
  qqExp x = [||x||]
instance QQExp L.Linkage L.Linkage where
  qqExp x = [||x||]
instance QQExp L.Visibility L.Visibility where
  qqExp x = [||x||]
instance QQExp L.AddrSpace L.AddrSpace where
  qqExp x = [||x||]
instance QQExp L.CallingConvention L.CallingConvention where
  qqExp x = [||x||]
instance QQExp L.ParameterAttribute L.ParameterAttribute where
  qqExp x = [||x||]
instance QQExp L.FunctionAttribute L.FunctionAttribute where
  qqExp x = [||x||]
instance QQExp LR.RMWOperation LR.RMWOperation where
  qqExp x = [||x||]
instance QQExp LI.IntegerPredicate LI.IntegerPredicate where
  qqExp x = [||x||]
instance QQExp LF.FloatingPointPredicate LF.FloatingPointPredicate where
  qqExp x = [||x||]
instance QQExp Bool Bool where
  qqExp x = [||x||]
instance QQExp Word Word where
  qqExp x = [||x||]
instance QQExp Word16 Word16 where
  qqExp x = [||x||]
instance QQExp Word32 Word32 where
  qqExp x = [||x||]
instance QQExp Word64 Word64 where
  qqExp x = [||x||]
instance QQExp Integer Integer where
  qqExp x = [||x||]
instance QQExp L.SomeFloat L.SomeFloat where
  qqExp x = [||x||]
instance QQExp [L.ParameterAttribute] [L.ParameterAttribute] where
  qqExp x = [||x||]
instance QQExp [L.FunctionAttribute] [L.FunctionAttribute] where
  qqExp x = [||x||]
instance QQExp [Word32] [Word32] where
  qqExp x = [||x||]

--instance (QQExp a b) => QQExp [a] [b] where
--  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
--  qqExp []     = [||[]||]

instance QQExp [A.MetadataNodeID] [L.MetadataNodeID] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [A.Named A.Instruction] [L.Named L.Instruction] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp A.InstructionMetadata L.InstructionMetadata where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [(A.Constant, A.Name)] [(L.Constant, L.Name)] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [A.Name] [L.Name] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [(A.Operand, [L.ParameterAttribute])] [(L.Operand, [L.ParameterAttribute])] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [A.Operand] [L.Operand] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [(A.Operand, A.Name)] [(L.Operand, L.Name)] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [A.LandingPadClause] [L.LandingPadClause] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [Maybe A.Operand] [Maybe L.Operand] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [A.Constant] [L.Constant] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [A.Type] [L.Type] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [(L.AddrSpace, (Word32, A.AlignmentInfo))] [(L.AddrSpace, (Word32, L.AlignmentInfo))] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance QQExp [((A.AlignType, Word32), A.AlignmentInfo)] [((L.AlignType, Word32), L.AlignmentInfo)] where
  qqExp (x:xs) = [||$$(qqExp x) : $$(qqExp xs)||]
  qqExp []     = [||[]||]

instance (QQExp a b) => QQExp (Maybe a) (Maybe b) where
  qqExp Nothing  = [||Nothing||]
  qqExp (Just x) = [||Just $$(qqExp x)||]

instance (QQExp a c, QQExp b d) => QQExp (Either a b) (Either c d) where
  qqExp (Left x)  = [||Left $$(qqExp x)||]
  qqExp (Right x) = [||Right $$(qqExp x)||]

instance (QQExp a c, QQExp b d) => QQExp (a,b) (c,d) where
  qqExp (x,y) = [||($$(qqExp x), $$(qqExp y))||]

--instance (QQExp a c, QQExp b d, Ord c) => QQExp (M.Map a b) (M.Map c d) where
--  qqExp = qqMapE

instance QQExp (M.Map L.AddrSpace (Word32, A.AlignmentInfo)) (M.Map L.AddrSpace (Word32, L.AlignmentInfo)) where
  qqExp = qqMapE

instance QQExp (M.Map (A.AlignType, Word32) A.AlignmentInfo) (M.Map (L.AlignType, Word32) L.AlignmentInfo) where
  qqExp = qqMapE

--instance (QQExp a b, Ord b) => QQExp (S.Set a) (S.Set b) where
--  qqExp = qqSetE

instance QQExp (S.Set Word32) (S.Set Word32) where
  qqExp = qqSetE

instance QQExp A.Definition L.Definition where
  qqExp = qqDefinitionE
instance QQExp [A.Definition] [L.Definition] where
  qqExp = qqDefinitionListE
instance QQExp A.Module L.Module where
  qqExp = qqModuleE
instance QQExp A.Global L.Global where
  qqExp = qqGlobalE
instance QQExp [A.Parameter] [L.Parameter] where
  qqExp = qqParameterListE
instance QQExp A.Parameter L.Parameter where
  qqExp = qqParameterE
instance QQExp A.BasicBlock L.BasicBlock where
  qqExp = qqBasicBlockE
instance QQExp [A.BasicBlock] [L.BasicBlock] where
  qqExp = qqBasicBlockListE
instance QQExp A.Terminator L.Terminator where
  qqExp = qqTerminatorE
instance QQExp A.MemoryOrdering L.MemoryOrdering where
  qqExp = qqMemoryOrderingE
instance QQExp A.Atomicity L.Atomicity where
  qqExp = qqAtomicityE
instance QQExp A.LandingPadClause L.LandingPadClause where
  qqExp = qqLandingPadClauseE
instance QQExp A.Instruction L.Instruction where
  qqExp = qqInstructionE
instance (QQExp a b) => QQExp (A.Named a) (L.Named b) where
  qqExp = qqNamedE
instance QQExp A.MetadataNodeID L.MetadataNodeID where
  qqExp = qqMetadataNodeIDE
instance QQExp A.MetadataNode L.MetadataNode where
  qqExp = qqMetadataNodeE
instance QQExp A.Operand L.Operand where
  qqExp = qqOperandE
instance QQExp A.Constant L.Constant where
  qqExp = qqConstantE
instance QQExp A.Name L.Name where
  qqExp = qqNameE
instance QQExp A.FloatingPointFormat L.FloatingPointFormat where
  qqExp = qqFloatingPointFormatE
instance QQExp A.Type L.Type where
  qqExp = qqTypeE
instance QQExp A.Dialect L.Dialect where
  qqExp = qqDialectE
instance QQExp A.InlineAssembly L.InlineAssembly where
  qqExp = qqInlineAssemblyE
instance QQExp A.Endianness L.Endianness where
  qqExp = qqEndiannessE
instance QQExp A.AlignmentInfo L.AlignmentInfo where
  qqExp = qqAlignmentInfoE
instance QQExp A.AlignType L.AlignType where
  qqExp = qqAlignTypeE
instance QQExp A.DataLayout L.DataLayout where
  qqExp = qqDataLayoutE
instance QQExp A.TargetTriple (Maybe String) where
  qqExp = qqTargetTripleE

qqDefinitionListE :: [A.Definition] -> TExpQ [L.Definition]
qqDefinitionListE [] = [||[]||]
qqDefinitionListE (A.AntiDefinitionList v : defs) =
    unsafeTExpCoerce [|toDefinitions $(unTypeQ $ antiVarE v) ++ $(unTypeQ (qqExp defs :: TExpQ [L.Definition]))|]
qqDefinitionListE (def : defs) =
    [||$$(qqExp def) : $$(qqExp defs)||]

qqDefinitionE :: A.Definition -> TExpQ L.Definition
qqDefinitionE (A.GlobalDefinition v) =
    [||L.GlobalDefinition $$(qqExp v) :: L.Definition||]
qqDefinitionE (A.TypeDefinition n v) =
    [||L.TypeDefinition $$(qqExp n) $$(qqExp v) :: L.Definition||]
qqDefinitionE (A.MetadataNodeDefinition i vs) =
    [||L.MetadataNodeDefinition $$(qqExp i) $$(qqExp vs) :: L.Definition||]
qqDefinitionE (A.NamedMetadataDefinition i vs) =
    [||L.NamedMetadataDefinition $$(qqExp i) $$(qqExp vs) :: L.Definition||]
qqDefinitionE (A.ModuleInlineAssembly s) =
    [||L.ModuleInlineAssembly $$(qqExp s) :: L.Definition||]
qqDefinitionE a@(A.AntiDefinition s) =
    antiVarE s
qqDefinitionE a@(A.AntiDefinitionList _s) =
    error $ "Internal Error: unexpected antiquote " ++ show a

qqModuleE :: A.Module -> TExpQ L.Module
qqModuleE (A.Module n dl tt ds) = 
  [||L.Module $$(qqExp n) $$(qqExp dl) $$(qqExp tt) $$(qqExp ds) :: L.Module||]

qqGlobalE :: A.Global -> TExpQ L.Global
qqGlobalE (A.GlobalVariable x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB) =
  [||L.GlobalVariable $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                          $$(qqExp x6) $$(qqExp x7) $$(qqExp x8) $$(qqExp x9) $$(qqExp xA)
                          $$(qqExp xB)||]
qqGlobalE (A.GlobalAlias x1 x2 x3 x4 x5) =
  [||L.GlobalAlias $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqGlobalE (A.Function x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC) =
  [||L.Function $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                    $$(qqExp x6) $$(qqExp x7) $$(qqExp x8) $$(qqExp x9) $$(qqExp xA)
                    $$(qqExp xB) $$(qqExp xC)||]

qqParameterListE :: [A.Parameter] -> TExpQ [L.Parameter]
qqParameterListE [] = [||[]||]
qqParameterListE (A.AntiParameterList v : defs) =
    [||$$(antiVarE v) ++ $$(qqExp defs)||]
qqParameterListE (def : defs) =
    [||$$(qqExp def) : $$(qqExp defs)||]

qqParameterE :: A.Parameter -> TExpQ L.Parameter
qqParameterE (A.Parameter x1 x2 x3) =
  [||L.Parameter $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqParameterE (A.AntiParameter s) =
  antiVarE s
qqParameterE a@(A.AntiParameterList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqBasicBlockListE :: [A.BasicBlock] -> TExpQ [L.BasicBlock]
qqBasicBlockListE [] = [||[]||]
qqBasicBlockListE (for@A.ForLoop{} : defs) =
  [||$$(qqExp $ transform for) ++ $$(qqExp defs)||]
qqBasicBlockListE (A.AntiBasicBlockList v : defs) =
  [||$$(antiVarE v) ++ $$(qqExp defs)||]
qqBasicBlockListE (def : defs) =
  [||$$(qqExp def) : $$(qqExp defs)||]

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
          , cond A.:= A.ICmp LI.ULE iter to []
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

qqBasicBlockE :: A.BasicBlock -> TExpQ L.BasicBlock
qqBasicBlockE (A.BasicBlock x1 x2 x3) =
  [||L.BasicBlock $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqBasicBlockE (A.AntiBasicBlock s) =
  antiVarE s
qqBasicBlockE a@A.ForLoop{} =
  error $ "Internal Error: unexpected loop " ++ show a
qqBasicBlockE a@(A.AntiBasicBlockList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqTerminatorE :: A.Terminator -> TExpQ L.Terminator
qqTerminatorE (A.Ret x1 x2) =
  [||L.Ret $$(qqExp x1) $$(qqExp x2)||]
qqTerminatorE (A.CondBr x1 x2 x3 x4) =
  [||L.CondBr $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqTerminatorE (A.Br x1 x2) =
  [||L.Br $$(qqExp x1) $$(qqExp x2)||]
qqTerminatorE (A.Switch x1 x2 x3 x4) =
  [||L.Switch $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqTerminatorE (A.IndirectBr x1 x2 x3) =
  [||L.IndirectBr $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqTerminatorE (A.Invoke x1 x2 x3 x4 x5 x6 x7 x8) =
  [||L.Invoke $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                  $$(qqExp x6) $$(qqExp x7) $$(qqExp x8)||]
qqTerminatorE (A.Resume x1 x2) =
  [||L.Resume $$(qqExp x1) $$(qqExp x2)||]
qqTerminatorE (A.Unreachable x1) =
  [||L.Unreachable $$(qqExp x1)||]

qqMemoryOrderingE :: A.MemoryOrdering -> TExpQ L.MemoryOrdering
qqMemoryOrderingE A.Unordered =
  [||L.Unordered||]
qqMemoryOrderingE A.Monotonic =
  [||L.Monotonic||]
qqMemoryOrderingE A.Acquire =
  [||L.Acquire||]
qqMemoryOrderingE A.Release =
  [||L.Release||]
qqMemoryOrderingE A.AcquireRelease =
  [||L.AcquireRelease||]
qqMemoryOrderingE A.SequentiallyConsistent =
  [||L.SequentiallyConsistent||]

qqAtomicityE :: A.Atomicity -> TExpQ L.Atomicity
qqAtomicityE (A.Atomicity x1 x2) =
  [||L.Atomicity $$(qqExp x1) $$(qqExp x2)||]

qqLandingPadClauseE :: A.LandingPadClause -> TExpQ L.LandingPadClause
qqLandingPadClauseE (A.Catch x1) =
  [||L.Catch $$(qqExp x1)||]
qqLandingPadClauseE (A.Filter x1) =
  [||L.Filter $$(qqExp x1)||]

qqInstructionE :: A.Instruction -> TExpQ L.Instruction
qqInstructionE (A.Add x1 x2 x3 x4 x5) =
  [||L.Add $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqInstructionE (A.FAdd x1 x2 x3) =
  [||L.FAdd $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Sub x1 x2 x3 x4 x5) =
  [||L.Sub $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqInstructionE (A.FSub x1 x2 x3) =
  [||L.FSub $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Mul x1 x2 x3 x4 x5) =
  [||L.Mul $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqInstructionE (A.FMul x1 x2 x3) =
  [||L.FMul $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.UDiv x1 x2 x3 x4) =
  [||L.UDiv $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.SDiv x1 x2 x3 x4) =
  [||L.SDiv $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.FDiv x1 x2 x3) =
  [||L.FDiv $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.URem x1 x2 x3) =
  [||L.URem $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.SRem x1 x2 x3) =
  [||L.SRem $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.FRem x1 x2 x3) =
  [||L.FRem $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Shl x1 x2 x3 x4 x5) =
  [||L.Shl $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqInstructionE (A.LShr x1 x2 x3 x4) =
  [||L.LShr $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.AShr x1 x2 x3 x4) =
  [||L.AShr $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.And x1 x2 x3) =
  [||L.And $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Or x1 x2 x3) =
  [||L.Or $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Xor x1 x2 x3) =
  [||L.Xor $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Alloca x1 x2 x3 x4) =
  [||L.Alloca $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.Load x1 x2 x3 x4 x5) =
  [||L.Load $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqInstructionE (A.Store x1 x2 x3 x4 x5 x6) =
  [||L.Store $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                 $$(qqExp x6)||]
qqInstructionE (A.GetElementPtr x1 x2 x3 x4) =
  [||L.GetElementPtr $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.Fence x1 x2) =
  [||L.Fence $$(qqExp x1) $$(qqExp x2)||]
qqInstructionE (A.CmpXchg x1 x2 x3 x4 x5 x6) =
  [||L.CmpXchg $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                  $$(qqExp x6)||]
qqInstructionE (A.AtomicRMW x1 x2 x3 x4 x5 x6) =
  [||L.AtomicRMW $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                     $$(qqExp x6)||]
qqInstructionE (A.Trunc x1 x2 x3) =
  [||L.Trunc $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.ZExt x1 x2 x3) =
  [||L.ZExt $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.SExt x1 x2 x3) =
  [||L.SExt $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.FPToUI x1 x2 x3) =
  [||L.FPToUI $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.FPToSI x1 x2 x3) =
  [||L.FPToSI $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.UIToFP x1 x2 x3) =
  [||L.UIToFP $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.SIToFP x1 x2 x3) =
  [||L.SIToFP $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.FPTrunc x1 x2 x3) =
  [||L.FPTrunc $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.FPExt x1 x2 x3) =
  [||L.FPExt $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.PtrToInt x1 x2 x3) =
  [||L.PtrToInt $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.IntToPtr x1 x2 x3) =
  [||L.IntToPtr $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.BitCast x1 x2 x3) =
  [||L.BitCast $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.AddrSpaceCast x1 x2 x3) =
  [||L.AddrSpaceCast $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.ICmp x1 x2 x3 x4) =
  [||L.ICmp $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.FCmp x1 x2 x3 x4) =
  [||L.FCmp $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.Phi x1 x2 x3) =
  [||L.Phi $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.Call x1 x2 x3 x4 x5 x6 x7) =
  [||L.Call $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                $$(qqExp x6) $$(qqExp x7)||]
qqInstructionE (A.Select x1 x2 x3 x4) =
  [||L.Select $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.VAArg x1 x2 x3) =
  [||L.VAArg $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.ExtractElement x1 x2 x3) =
  [||L.ExtractElement $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.InsertElement x1 x2 x3 x4) =
  [||L.InsertElement $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.ShuffleVector x1 x2 x3 x4) =
  [||L.ShuffleVector $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.ExtractValue x1 x2 x3) =
  [||L.ExtractValue $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqInstructionE (A.InsertValue x1 x2 x3 x4) =
  [||L.InsertValue $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4)||]
qqInstructionE (A.LandingPad x1 x2 x3 x4 x5) =
  [||L.LandingPad $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqInstructionE (A.AntiInstruction s) =
  antiVarE s

qqNamedE :: (QQExp a b) => A.Named a -> TExpQ (L.Named b)
qqNamedE ((A.:=) x1 x2) =
  [||(L.:=) $$(qqExp x1) $$(qqExp x2)||]
qqNamedE (A.Do x1) =
  [||L.Do $$(qqExp x1)||]

qqMetadataNodeIDE :: A.MetadataNodeID -> TExpQ L.MetadataNodeID
qqMetadataNodeIDE (A.MetadataNodeID x1) =
  [||L.MetadataNodeID $$(qqExp x1)||]

qqMetadataNodeE :: A.MetadataNode -> TExpQ L.MetadataNode
qqMetadataNodeE (A.MetadataNode x1) =
  [||L.MetadataNode $$(qqExp x1)||]
qqMetadataNodeE (A.MetadataNodeReference x1) =
  [||L.MetadataNodeReference $$(qqExp x1)||]

qqOperandE :: A.Operand -> TExpQ L.Operand
qqOperandE (A.LocalReference x1) =
  [||L.LocalReference $$(qqExp x1)||]
qqOperandE (A.ConstantOperand x1) =
  [||L.ConstantOperand $$(qqExp x1)||]
qqOperandE (A.MetadataStringOperand x1) =
  [||L.MetadataStringOperand $$(qqExp x1)||]
qqOperandE (A.MetadataNodeOperand x1) =
  [||L.MetadataNodeOperand $$(qqExp x1)||]
qqOperandE (A.AntiOperand s) =
  antiVarE s

qqConstantE :: A.Constant -> TExpQ L.Constant
qqConstantE (A.Int x1 x2) =
  [||L.Int $$(qqExp x1) $$(qqExp x2)||]
qqConstantE (A.Float x1) =
  [||L.Float $$(qqExp x1)||]
qqConstantE (A.Null x1) =
  [||L.Null $$(qqExp x1)||]
qqConstantE (A.Struct x1 x2 x3) =
  [||L.Struct $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqConstantE (A.Array x1 x2) =
  [||L.Array $$(qqExp x1) $$(qqExp x2)||]
qqConstantE (A.Vector x1) =
  [||L.Vector $$(qqExp x1)||]
qqConstantE (A.Undef x1) =
  [||L.Undef $$(qqExp x1)||]
qqConstantE (A.BlockAddress x1 x2) =
  [||L.BlockAddress $$(qqExp x1) $$(qqExp x2)||]
qqConstantE (A.GlobalReference x1) =
  [||L.GlobalReference $$(qqExp x1)||]
qqConstantE (A.AntiConstant s) =
  unsafeTExpCoerce [|toConstant $(unTypeQ $ antiVarE s)|]

qqNameE :: A.Name -> TExpQ L.Name
qqNameE (A.Name x1) =
  [||L.Name $$(qqExp x1)||]
qqNameE (A.UnName x1) =
  [||L.UnName $$(qqExp x1)||]
qqNameE (A.AntiName s) =
  unsafeTExpCoerce [|toName $(unTypeQ $ antiVarE s)|]

qqFloatingPointFormatE :: A.FloatingPointFormat -> TExpQ L.FloatingPointFormat
qqFloatingPointFormatE A.IEEE =
  [||L.IEEE||]
qqFloatingPointFormatE A.DoubleExtended =
  [||L.DoubleExtended||]
qqFloatingPointFormatE A.PairOfFloats =
  [||L.PairOfFloats||]

qqTypeE :: A.Type -> TExpQ L.Type
qqTypeE A.VoidType =
  [||L.VoidType||]
qqTypeE (A.IntegerType x1) =
  [||L.IntegerType $$(qqExp x1)||]
qqTypeE (A.PointerType x1 x2) =
  [||L.PointerType $$(qqExp x1) $$(qqExp x2)||]
qqTypeE (A.FloatingPointType x1 x2) =
  [||L.FloatingPointType $$(qqExp x1) $$(qqExp x2)||]
qqTypeE (A.FunctionType x1 x2 x3) =
  [||L.FunctionType $$(qqExp x1) $$(qqExp x2) $$(qqExp x3)||]
qqTypeE (A.VectorType x1 x2) =
  [||L.VectorType $$(qqExp x1) $$(qqExp x2)||]
qqTypeE (A.StructureType x1 x2) =
  [||L.StructureType $$(qqExp x1) $$(qqExp x2)||]
qqTypeE (A.ArrayType x1 x2) =
  [||L.ArrayType $$(qqExp x1) $$(qqExp x2)||]
qqTypeE (A.NamedTypeReference x1) =
  [||L.NamedTypeReference $$(qqExp x1)||]
qqTypeE A.MetadataType =
  [||L.MetadataType||]
qqTypeE (A.AntiType s) =
  antiVarE s

qqDialectE :: A.Dialect -> TExpQ L.Dialect
qqDialectE A.ATTDialect =
  [||L.ATTDialect||]
qqDialectE A.IntelDialect =
  [||L.IntelDialect||]

qqInlineAssemblyE :: A.InlineAssembly -> TExpQ L.InlineAssembly
qqInlineAssemblyE (A.InlineAssembly x1 x2 x3 x4 x5 x6) =
  [||L.InlineAssembly $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)
                          $$(qqExp x6)||]

qqMapE :: (QQExp [(a, b)] [(c, d)], Ord c) => M.Map a b -> TExpQ (M.Map c d)
qqMapE m =
  [||M.fromList $$(qqExp (M.toList m))||]

qqSetE :: (QQExp [a] [b], Ord b) => S.Set a -> TExpQ (S.Set b)
qqSetE m =
  [||S.fromList $$(qqExp (S.toList m))||]

qqEndiannessE :: A.Endianness -> TExpQ L.Endianness
qqEndiannessE A.LittleEndian =
  [||L.LittleEndian||]
qqEndiannessE A.BigEndian =
  [||L.BigEndian||]

qqAlignmentInfoE :: A.AlignmentInfo -> TExpQ L.AlignmentInfo
qqAlignmentInfoE (A.AlignmentInfo x1 x2) =
  [||L.AlignmentInfo $$(qqExp x1) $$(qqExp x2)||]

qqAlignTypeE :: A.AlignType -> TExpQ L.AlignType
qqAlignTypeE A.IntegerAlign =
  [||L.IntegerAlign||]
qqAlignTypeE A.VectorAlign =
  [||L.VectorAlign||]
qqAlignTypeE A.FloatAlign =
  [||L.FloatAlign||]
qqAlignTypeE A.AggregateAlign =
  [||L.AggregateAlign||]
qqAlignTypeE A.StackAlign =
  [||L.StackAlign||]

qqDataLayoutE :: A.DataLayout -> TExpQ L.DataLayout
qqDataLayoutE (A.DataLayout x1 x2 x3 x4 x5) =
  [||L.DataLayout $$(qqExp x1) $$(qqExp x2) $$(qqExp x3) $$(qqExp x4) $$(qqExp x5)||]
qqDataLayoutE (A.AntiDataLayout s) =
  antiVarE s

qqTargetTripleE :: A.TargetTriple -> TExpQ (Maybe String)
qqTargetTripleE A.NoTargetTriple =
  [||Nothing||]
qqTargetTripleE (A.TargetTriple v) =
  [||Just $$(qqExp v)||]
qqTargetTripleE (A.AntiTargetTriple v) =
  unsafeTExpCoerce [|toTargetTriple $(unTypeQ $ antiVarE v)|]

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

newtype TQuasiQuoter a = TQuasiQuoter { unTQuasiQuoter :: QuasiQuoter }

quasiquote :: forall a b. (Data a, QQExp a b)
           => [A.Extensions]
           -> P.P a
           -> TQuasiQuoter b
quasiquote exts p = TQuasiQuoter $
    QuasiQuoter { quoteExp  = parse exts p >=> unTypeQ . (qqExp :: a -> TExpQ b)
                , quotePat  = parse exts p >=> qqP
                , quoteType = fail "LLVM type quasiquoter undefined"
                , quoteDec  = fail "LLVM declaration quasiquoter undefined"
                }
