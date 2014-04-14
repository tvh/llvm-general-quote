{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LLVM.General.Quote.Base (
    CodeGenMonad(..),
    ToDefintions(..),
    quasiquote,
    quasiquoteM,
    TQuasiQuoter(..),
    parse
  ) where

import Control.Applicative
import Control.Monad.Identity
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
import Data.IORef (atomicModifyIORef')
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToPatQ)

import qualified LLVM.General.Quote.Parser as P
import qualified LLVM.General.Quote.AST as A
import qualified LLVM.General.AST.IntegerPredicate as LI
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Constant as L
  (Constant(Int, Float, Null, Struct, Array, Vector,
            Undef, BlockAddress, GlobalReference))
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

class Monad m => CodeGenMonad m where
  data Variable m
  newVariable :: m (Variable m)
  getVariable :: Variable m -> m [L.Operand]
  (.=.) :: Variable m -> m [L.Operand] -> m [L.BasicBlock]
  exec :: m () -> m [L.BasicBlock]

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
antiVarE s = [|$(either fail return $ parseExp s)|]

type Conversion a b = forall m.(Applicative m, Monad m) => a -> TExpQ (m b)
type Conversion' m a b = (Applicative m, Monad m) => a -> TExpQ (m b)

class QQExp a b where
  qqExpM :: Conversion a b
  qqExp :: a -> TExpQ b
  qqExp x = [||runIdentity $$(qqExpM x)||]

--instance (Lift a) => QQExp a a where
--  qqExpM x = [||x||]
instance QQExp String String where
  qqExpM x = [||pure x||]
instance QQExp L.Linkage L.Linkage where
  qqExpM x = [||pure x||]
instance QQExp L.Visibility L.Visibility where
  qqExpM x = [||pure x||]
instance QQExp L.AddrSpace L.AddrSpace where
  qqExpM x = [||pure x||]
instance QQExp L.CallingConvention L.CallingConvention where
  qqExpM x = [||pure x||]
instance QQExp L.ParameterAttribute L.ParameterAttribute where
  qqExpM x = [||pure x||]
instance QQExp L.FunctionAttribute L.FunctionAttribute where
  qqExpM x = [||pure x||]
instance QQExp LR.RMWOperation LR.RMWOperation where
  qqExpM x = [||pure x||]
instance QQExp LI.IntegerPredicate LI.IntegerPredicate where
  qqExpM x = [||pure x||]
instance QQExp LF.FloatingPointPredicate LF.FloatingPointPredicate where
  qqExpM x = [||pure x||]
instance QQExp Bool Bool where
  qqExpM x = [||pure x||]
instance QQExp Word Word where
  qqExpM x = [||pure x||]
instance QQExp Word16 Word16 where
  qqExpM x = [||pure x||]
instance QQExp Word32 Word32 where
  qqExpM x = [||pure x||]
instance QQExp Word64 Word64 where
  qqExpM x = [||pure x||]
instance QQExp Integer Integer where
  qqExpM x = [||pure x||]
instance QQExp L.SomeFloat L.SomeFloat where
  qqExpM x = [||pure x||]
instance QQExp [L.ParameterAttribute] [L.ParameterAttribute] where
  qqExpM x = [||pure x||]
instance QQExp [L.FunctionAttribute] [L.FunctionAttribute] where
  qqExpM x = [||pure x||]
instance QQExp [Word32] [Word32] where
  qqExpM x = [||pure x||]

--instance (QQExp a b) => QQExp [a] [b] where
--  qqExpM (x:xs) = [||$$(qqExpM x) : $$(qqExpM xs)||]
--  qqExpM []     = [||[]||]

instance QQExp [A.MetadataNodeID] [L.MetadataNodeID] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp A.InstructionMetadata L.InstructionMetadata where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(A.Constant, A.Name)] [(L.Constant, L.Name)] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Name] [L.Name] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(A.Operand, [L.ParameterAttribute])]
               [(L.Operand, [L.ParameterAttribute])] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Operand] [L.Operand] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(A.Operand, A.Name)] [(L.Operand, L.Name)] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.LandingPadClause] [L.LandingPadClause] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [Maybe A.Operand] [Maybe L.Operand] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Constant] [L.Constant] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Type] [L.Type] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(L.AddrSpace, (Word32, A.AlignmentInfo))]
               [(L.AddrSpace, (Word32, L.AlignmentInfo))] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [((A.AlignType, Word32), A.AlignmentInfo)]
               [((L.AlignType, Word32), L.AlignmentInfo)] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance (QQExp a b) => QQExp (Maybe a) (Maybe b) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance (QQExp a c, QQExp b d) => QQExp (Either a b) (Either c d) where
  qqExpM (Left x)  = [||Left <$> $$(qqExpM x)||]
  qqExpM (Right x) = [||Right <$> $$(qqExpM x)||]

instance (QQExp a c, QQExp b d) => QQExp (a,b) (c,d) where
  qqExpM (x,y) = [||(,) <$> $$(qqExpM x) <*> $$(qqExpM y)||]

instance (QQExp a d, QQExp b e, QQExp c f) => QQExp (a,b,c) (d,e,f) where
  qqExpM (x,y,z) = [||(,,) <$> $$(qqExpM x) <*> $$(qqExpM y) <*> $$(qqExpM z)||]

--instance (QQExp a c, QQExp b d, Ord c) => QQExp (M.Map a b) (M.Map c d) where
--  qqExpM = qqMapE

instance QQExp (M.Map L.AddrSpace (Word32, A.AlignmentInfo))
               (M.Map L.AddrSpace (Word32, L.AlignmentInfo)) where
  qqExpM = qqMapE

instance QQExp (M.Map (A.AlignType, Word32) A.AlignmentInfo)
               (M.Map (L.AlignType, Word32) L.AlignmentInfo) where
  qqExpM = qqMapE

--instance (QQExp a b, Ord b) => QQExp (S.Set a) (S.Set b) where
--  qqExpM = qqSetE

instance QQExp (S.Set Word32) (S.Set Word32) where
  qqExpM = qqSetE

instance QQExp A.Definition L.Definition where
  qqExpM = qqDefinitionE
instance QQExp [A.Definition] [L.Definition] where
  qqExpM = qqDefinitionListE
instance QQExp A.Module L.Module where
  qqExpM = qqModuleE
instance QQExp A.Global L.Global where
  qqExpM = qqGlobalE
instance QQExp [A.Parameter] [L.Parameter] where
  qqExpM = qqParameterListE
instance QQExp A.Parameter L.Parameter where
  qqExpM = qqParameterE
instance QQExp A.BasicBlock L.BasicBlock where
  qqExpM = qqBasicBlockE
instance QQExp [A.BasicBlock] [L.BasicBlock] where
  qqExpM = qqBasicBlockListE
instance QQExp A.Terminator L.Terminator where
  qqExpM = qqTerminatorE
instance QQExp A.MemoryOrdering L.MemoryOrdering where
  qqExpM = qqMemoryOrderingE
instance QQExp A.Atomicity L.Atomicity where
  qqExpM = qqAtomicityE
instance QQExp A.LandingPadClause L.LandingPadClause where
  qqExpM = qqLandingPadClauseE
instance QQExp A.Instruction L.Instruction where
  qqExpM = qqInstructionE
instance QQExp [A.Named A.Instruction] [L.Named L.Instruction] where
  qqExpM = qqNamedInstructionListE
instance (QQExp a b) => QQExp (A.Named a) (L.Named b) where
  qqExpM = qqNamedE
instance QQExp A.MetadataNodeID L.MetadataNodeID where
  qqExpM = qqMetadataNodeIDE
instance QQExp A.MetadataNode L.MetadataNode where
  qqExpM = qqMetadataNodeE
instance QQExp A.Operand L.Operand where
  qqExpM = qqOperandE
instance QQExp A.Constant L.Constant where
  qqExpM = qqConstantE
instance QQExp A.Name L.Name where
  qqExpM = qqNameE
instance QQExp A.FloatingPointFormat L.FloatingPointFormat where
  qqExpM = qqFloatingPointFormatE
instance QQExp A.Type L.Type where
  qqExpM = qqTypeE
instance QQExp A.Dialect L.Dialect where
  qqExpM = qqDialectE
instance QQExp A.InlineAssembly L.InlineAssembly where
  qqExpM = qqInlineAssemblyE
instance QQExp A.Endianness L.Endianness where
  qqExpM = qqEndiannessE
instance QQExp A.AlignmentInfo L.AlignmentInfo where
  qqExpM = qqAlignmentInfoE
instance QQExp A.AlignType L.AlignType where
  qqExpM = qqAlignTypeE
instance QQExp A.DataLayout L.DataLayout where
  qqExpM = qqDataLayoutE
instance QQExp A.TargetTriple (Maybe String) where
  qqExpM = qqTargetTripleE

qqDefinitionListE :: Conversion [A.Definition] [L.Definition]
qqDefinitionListE [] = [||pure []||]
qqDefinitionListE (A.AntiDefinitionList v : defs) =
    [||(++) <$> $$(unsafeTExpCoerce [|$(antiVarE v) >>= return . toDefinitions|])
       <*> $$(qqExpM defs)||]
qqDefinitionListE (def : defs) =
    [||(:) <$> $$(qqExpM def) <*> $$(qqExpM defs)||]

qqDefinitionE :: Conversion A.Definition L.Definition
qqDefinitionE (A.GlobalDefinition v) =
    [||L.GlobalDefinition <$> $$(qqExpM v)||]
qqDefinitionE (A.TypeDefinition n v) =
    [||L.TypeDefinition <$> $$(qqExpM n) <*> $$(qqExpM v)||]
qqDefinitionE (A.MetadataNodeDefinition i vs) =
    [||L.MetadataNodeDefinition <$> $$(qqExpM i) <*> $$(qqExpM vs)||]
qqDefinitionE (A.NamedMetadataDefinition i vs) =
    [||L.NamedMetadataDefinition <$> $$(qqExpM i) <*> $$(qqExpM vs)||]
qqDefinitionE (A.ModuleInlineAssembly s) =
    [||L.ModuleInlineAssembly <$> $$(qqExpM s)||]
qqDefinitionE (A.AntiDefinition s) =
    unsafeTExpCoerce $ [|$(antiVarE s) >>= return . toDefinition|]
qqDefinitionE a@(A.AntiDefinitionList _s) =
    error $ "Internal Error: unexpected antiquote " ++ show a

qqModuleE :: Conversion A.Module L.Module
qqModuleE (A.Module n dl tt ds) =
  [||L.Module <$> $$(qqExpM n) <*> $$(qqExpM dl) <*> $$(qqExpM tt) <*> $$(qqExpM ds)||]

qqGlobalE :: Conversion A.Global L.Global
qqGlobalE (A.GlobalVariable x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB) =
  [||L.GlobalVariable <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                      <*> $$(qqExpM x5) <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8)
                      <*> $$(qqExpM x9) <*> $$(qqExpM xA) <*> $$(qqExpM xB)||]
qqGlobalE (A.GlobalAlias x1 x2 x3 x4 x5) =
  [||L.GlobalAlias <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                   <*> $$(qqExpM x5)||]
qqGlobalE (A.Function x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC) =
  [||L.Function <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                <*> $$(qqExpM x5) <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8)
                <*> $$(qqExpM x9) <*> $$(qqExpM xA) <*> $$(qqExpM xB) <*> $$(qqExpM xC)||]

qqParameterListE :: Conversion [A.Parameter] [L.Parameter]
qqParameterListE [] = [||pure []||]
qqParameterListE (A.AntiParameterList v : defs) =
    [||(++) <$> $$(unsafeTExpCoerce $ antiVarE v) <*> $$(qqExpM defs)||]
qqParameterListE (def : defs) =
    [||(:) <$> $$(qqExpM def) <*> $$(qqExpM defs)||]

qqParameterE :: Conversion A.Parameter L.Parameter
qqParameterE (A.Parameter x1 x2 x3) =
  [||L.Parameter <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqParameterE (A.AntiParameter s) =
  unsafeTExpCoerce $ antiVarE s
qqParameterE a@(A.AntiParameterList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqBasicBlockListE :: Conversion [A.BasicBlock] [L.BasicBlock]
qqBasicBlockListE [] = [||pure []||]
qqBasicBlockListE (def : defs) =
  [||(++) <$> $$(transform def) <*> $$(qqExpM defs)||]

transform :: forall m.Conversion' m A.BasicBlock [L.BasicBlock]
transform bb@A.BasicBlock{} = [||(:[]) <$> $$(qqExpM bb)||]
transform (A.ForLoop label iterType iterName from to step element body next) =
  [||
    let ret :: L.BasicBlock -> Maybe (Maybe L.Operand, L.Name)
        ret (L.BasicBlock l _ t') = do
          let t = case t' of
                    _ L.:= t'' -> t''
                    L.Do t''   -> t''
          L.Ret x _ <- return t
          return (x,l)

        replaceRets :: L.Name -> [L.BasicBlock] -> [L.BasicBlock]
        replaceRets _ [] = []
        replaceRets n (x:xs) = replaceRet n x : replaceRets n xs

        replaceRet :: L.Name -> L.BasicBlock -> L.BasicBlock
        replaceRet labelR bb@(L.BasicBlock bbn is t) =
          case t of
            n L.:= L.Ret _ md -> L.BasicBlock bbn is (n L.:= L.Br labelR md)
            L.Do (L.Ret _ md) -> L.BasicBlock bbn is (L.Do (L.Br labelR md))
            _                 -> bb

        iterName' = $$(qqExpM iterName :: TExpQ (m L.Name))
        iterType' = $$(qqExpM iterType :: TExpQ (m L.Type))
        from' = $$(qqExpM from :: TExpQ (m L.Operand))
        step' = $$(qqExpM step :: TExpQ (m L.Operand))
        element' = $$(qqExpM element :: TExpQ (m (Either [L.Name] (L.Type, [(L.Operand, L.Name)], L.Name))))
        mElementF e = case e of
                     Left  _ -> Nothing
                     Right x -> Just x
        mElement = mElementF <$> element'
        phiElementF e returns' =
          case e of
            Left _ -> []
            Right (elementType,elementFrom,elementName) ->
              let returns'' = [(x,l) | (Just x, l) <- returns']
              in [elementName L.:= L.Phi elementType (returns'' ++ elementFrom) []]
        phiElement = phiElementF <$> element' <*> returns
        label' = $$(qqExpM label :: TExpQ (m L.Name))
        labelStringF l = case l of
                        L.Name s -> s
                        L.UnName n -> "num"++show n
        labelString = labelStringF <$> label'
        cond = L.Name <$> ((++) <$> labelString <*> pure ".cond")
        labelEnd = L.Name <$> ((++) <$> labelString <*> pure ".end")
        iterNameNewF l = L.Name $ case l of
                        L.Name s -> s ++ ".new"
                        L.UnName n -> "num"++show n++".new"
        iterNameNew = iterNameNewF <$> $$(qqExpM iterName)
        iterBitsF t = case t of
                     L.IntegerType n -> n
                     t -> error $ "Internal Error: unexpected type " ++ show t
        iterBits = iterBitsF <$> iterType'
        iter = (L.LocalReference <$> $$(qqExpM iterName))
        newItersF n rs = map (\(_,l) -> (L.LocalReference n,l)) rs
        newIters = newItersF <$> iterNameNew <*> returns
        initFromsF e = case e of
                     Left ns  -> ns
                     Right (_,xs,_) -> map snd xs
        initFroms = initFromsF <$> element'
        initIterF from' initFroms = map (\s -> (from',s)) initFroms
        initIter = initIterF <$> from' <*> initFroms
        preInstrsF iterName' iterType' newIters initIter phiElement cond iter to iterNameNew step' =
          [ iterName' L.:= L.Phi iterType' (newIters ++ initIter) [] ]
          ++ phiElement ++
          [ cond L.:= L.ICmp LI.ULE iter to []
          , iterNameNew L.:= L.Add True True iter step' []
          ]
        preInstrs = preInstrsF <$> iterName' <*> iterType' <*> newIters <*> initIter <*> phiElement <*> cond <*> iter <*> $$(qqExpM to) <*> iterNameNew <*> step'
        body' = $$(qqExpM body :: TExpQ (m [L.BasicBlock]))
        returns = ((>>=) <$> body' <*> pure (maybeToList . ret))
        --branchTo :: L.Name -> m (L.Named L.Terminator)
        branchTo l = (body' >>= \(L.BasicBlock bodyLabel _ _:_) -> L.Do <$> (L.CondBr <$> (L.LocalReference <$> cond) <*> pure bodyLabel <*> pure l <*> pure []))
        retElement = ((>>=) <$> mElement <*> pure (\(_,_,n) -> return $ L.LocalReference n))
        retTerm = (L.Do <$> (L.Ret <$> retElement <*> pure []))
        pre = do
          next'' <- $$(qqExpM next)
          l <- label'
          lE <- labelEnd
          pIs <- preInstrs
          case next'' of
            Just next' -> do
              bNext <- branchTo next'
              return [L.BasicBlock l pIs bNext]
            Nothing -> do
              bEnd <- (branchTo lE)
              rT <- retTerm
              return $
                [ L.BasicBlock l pIs bEnd
                , L.BasicBlock lE [] rT
                ]
        main = replaceRets <$> label' <*> body'
    in (++) <$> pre <*> main
  ||]
transform (A.AntiBasicBlock v)
  = [||(:[]) <$> $$(unsafeTExpCoerce $ antiVarE v)||]
transform (A.AntiBasicBlockList v)
  = unsafeTExpCoerce $ antiVarE v

qqBasicBlockE :: Conversion A.BasicBlock L.BasicBlock
qqBasicBlockE (A.BasicBlock x1 x2 x3) =
  [||L.BasicBlock <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqBasicBlockE (A.AntiBasicBlock s) =
  unsafeTExpCoerce $ antiVarE s
qqBasicBlockE a@A.ForLoop{} =
  error $ "Internal Error: unexpected loop " ++ show a
qqBasicBlockE a@(A.AntiBasicBlockList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqTerminatorE :: Conversion A.Terminator L.Terminator
qqTerminatorE (A.Ret x1 x2) =
  [||L.Ret <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTerminatorE (A.CondBr x1 x2 x3 x4) =
  [||L.CondBr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqTerminatorE (A.Br x1 x2) =
  [||L.Br <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTerminatorE (A.Switch x1 x2 x3 x4) =
  [||L.Switch <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqTerminatorE (A.IndirectBr x1 x2 x3) =
  [||L.IndirectBr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqTerminatorE (A.Invoke x1 x2 x3 x4 x5 x6 x7 x8) =
  [||L.Invoke <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
              <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8)||]
qqTerminatorE (A.Resume x1 x2) =
  [||L.Resume <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTerminatorE (A.Unreachable x1) =
  [||L.Unreachable <$> $$(qqExpM x1)||]

qqMemoryOrderingE :: Conversion A.MemoryOrdering L.MemoryOrdering
qqMemoryOrderingE A.Unordered =
  [||pure L.Unordered||]
qqMemoryOrderingE A.Monotonic =
  [||pure L.Monotonic||]
qqMemoryOrderingE A.Acquire =
  [||pure L.Acquire||]
qqMemoryOrderingE A.Release =
  [||pure L.Release||]
qqMemoryOrderingE A.AcquireRelease =
  [||pure L.AcquireRelease||]
qqMemoryOrderingE A.SequentiallyConsistent =
  [||pure L.SequentiallyConsistent||]

qqAtomicityE :: Conversion A.Atomicity L.Atomicity
qqAtomicityE (A.Atomicity x1 x2) =
  [||L.Atomicity <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]

qqLandingPadClauseE :: Conversion A.LandingPadClause L.LandingPadClause
qqLandingPadClauseE (A.Catch x1) =
  [||L.Catch <$> $$(qqExpM x1)||]
qqLandingPadClauseE (A.Filter x1) =
  [||L.Filter <$> $$(qqExpM x1)||]

qqInstructionE :: Conversion A.Instruction L.Instruction
qqInstructionE (A.Add x1 x2 x3 x4 x5) =
  [||L.Add <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)||]
qqInstructionE (A.FAdd x1 x2 x3) =
  [||L.FAdd <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Sub x1 x2 x3 x4 x5) =
  [||L.Sub <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)||]
qqInstructionE (A.FSub x1 x2 x3) =
  [||L.FSub <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Mul x1 x2 x3 x4 x5) =
  [||L.Mul <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)||]
qqInstructionE (A.FMul x1 x2 x3) =
  [||L.FMul <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.UDiv x1 x2 x3 x4) =
  [||L.UDiv <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.SDiv x1 x2 x3 x4) =
  [||L.SDiv <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.FDiv x1 x2 x3) =
  [||L.FDiv <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.URem x1 x2 x3) =
  [||L.URem <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.SRem x1 x2 x3) =
  [||L.SRem <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.FRem x1 x2 x3) =
  [||L.FRem <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Shl x1 x2 x3 x4 x5) =
  [||L.Shl <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)||]
qqInstructionE (A.LShr x1 x2 x3 x4) =
  [||L.LShr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.AShr x1 x2 x3 x4) =
  [||L.AShr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.And x1 x2 x3) =
  [||L.And <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Or x1 x2 x3) =
  [||L.Or <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Xor x1 x2 x3) =
  [||L.Xor <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Alloca x1 x2 x3 x4) =
  [||L.Alloca <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.Load x1 x2 x3 x4 x5) =
  [||L.Load <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)||]
qqInstructionE (A.Store x1 x2 x3 x4 x5 x6) =
  [||L.Store <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
             <*> $$(qqExpM x6)||]
qqInstructionE (A.GetElementPtr x1 x2 x3 x4) =
  [||L.GetElementPtr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.Fence x1 x2) =
  [||L.Fence <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqInstructionE (A.CmpXchg x1 x2 x3 x4 x5 x6) =
  [||L.CmpXchg <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
               <*> $$(qqExpM x6)||]
qqInstructionE (A.AtomicRMW x1 x2 x3 x4 x5 x6) =
  [||L.AtomicRMW <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                 <*> $$(qqExpM x5) <*> $$(qqExpM x6)||]
qqInstructionE (A.Trunc x1 x2 x3) =
  [||L.Trunc <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.ZExt x1 x2 x3) =
  [||L.ZExt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.SExt x1 x2 x3) =
  [||L.SExt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.FPToUI x1 x2 x3) =
  [||L.FPToUI <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.FPToSI x1 x2 x3) =
  [||L.FPToSI <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.UIToFP x1 x2 x3) =
  [||L.UIToFP <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.SIToFP x1 x2 x3) =
  [||L.SIToFP <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.FPTrunc x1 x2 x3) =
  [||L.FPTrunc <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.FPExt x1 x2 x3) =
  [||L.FPExt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.PtrToInt x1 x2 x3) =
  [||L.PtrToInt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.IntToPtr x1 x2 x3) =
  [||L.IntToPtr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.BitCast x1 x2 x3) =
  [||L.BitCast <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.AddrSpaceCast x1 x2 x3) =
  [||L.AddrSpaceCast <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.ICmp x1 x2 x3 x4) =
  [||L.ICmp <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.FCmp x1 x2 x3 x4) =
  [||L.FCmp <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.Phi x1 x2 x3) =
  [||L.Phi <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.Call x1 x2 x3 x4 x5 x6 x7) =
  [||L.Call <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
            <*> $$(qqExpM x6) <*> $$(qqExpM x7)||]
qqInstructionE (A.Select x1 x2 x3 x4) =
  [||L.Select <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.VAArg x1 x2 x3) =
  [||L.VAArg <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.ExtractElement x1 x2 x3) =
  [||L.ExtractElement <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.InsertElement x1 x2 x3 x4) =
  [||L.InsertElement <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.ShuffleVector x1 x2 x3 x4) =
  [||L.ShuffleVector <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.ExtractValue x1 x2 x3) =
  [||L.ExtractValue <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqInstructionE (A.InsertValue x1 x2 x3 x4) =
  [||L.InsertValue <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]
qqInstructionE (A.LandingPad x1 x2 x3 x4 x5) =
  [||L.LandingPad <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                  <*> $$(qqExpM x5)||]
qqInstructionE (A.AntiInstruction s) =
  unsafeTExpCoerce $ antiVarE s

qqNamedInstructionListE :: Conversion [A.Named A.Instruction] [L.Named L.Instruction]
qqNamedInstructionListE [] =
  [||pure []||]
qqNamedInstructionListE (A.AntiInstructionList s:xs) =
  [||(++) <$> $$(unsafeTExpCoerce $ antiVarE s) <*> $$(qqExpM xs)||]
qqNamedInstructionListE (x:xs) =
  [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]

qqNamedE :: (QQExp a b) => Conversion (A.Named a) (L.Named b)
qqNamedE ((A.:=) x1 x2) =
  [||(L.:=) <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqNamedE (A.Do x1) =
  [||L.Do <$> $$(qqExpM x1)||]
qqNamedE (A.AntiInstructionList s) =
  error $ "internal error: unexpected anti-quotation" ++ s

qqMetadataNodeIDE :: Conversion A.MetadataNodeID L.MetadataNodeID
qqMetadataNodeIDE (A.MetadataNodeID x1) =
  [||L.MetadataNodeID <$> $$(qqExpM x1)||]

qqMetadataNodeE :: Conversion A.MetadataNode L.MetadataNode
qqMetadataNodeE (A.MetadataNode x1) =
  [||L.MetadataNode <$> $$(qqExpM x1)||]
qqMetadataNodeE (A.MetadataNodeReference x1) =
  [||L.MetadataNodeReference <$> $$(qqExpM x1)||]

qqOperandE :: Conversion A.Operand L.Operand
qqOperandE (A.LocalReference x1) =
  [||L.LocalReference <$> $$(qqExpM x1)||]
qqOperandE (A.ConstantOperand x1) =
  [||L.ConstantOperand <$> $$(qqExpM x1)||]
qqOperandE (A.MetadataStringOperand x1) =
  [||L.MetadataStringOperand <$> $$(qqExpM x1)||]
qqOperandE (A.MetadataNodeOperand x1) =
  [||L.MetadataNodeOperand <$> $$(qqExpM x1)||]
qqOperandE (A.AntiOperand s) =
  unsafeTExpCoerce $ antiVarE s

qqConstantE :: Conversion A.Constant L.Constant
qqConstantE (A.Int x1 x2) =
  [||L.Int <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqConstantE (A.IntAntiBs x1 x2) =
  [||let typeBits (L.IntegerType bs) = return bs
         typeBits t                  = error $ "unexpected type: " ++ show t
     in L.Int <$> ($$(unsafeTExpCoerce (antiVarE x1)) >>= typeBits) <*> $$(qqExpM x2)||]
qqConstantE (A.Float x1) =
  [||L.Float <$> $$(qqExpM x1)||]
qqConstantE (A.Null x1) =
  [||L.Null <$> $$(qqExpM x1)||]
qqConstantE (A.Struct x1 x2 x3) =
  [||L.Struct <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqConstantE (A.Array x1 x2) =
  [||L.Array <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqConstantE (A.Vector x1) =
  [||L.Vector <$> $$(qqExpM x1)||]
qqConstantE (A.Undef x1) =
  [||L.Undef <$> $$(qqExpM x1)||]
qqConstantE (A.BlockAddress x1 x2) =
  [||L.BlockAddress <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqConstantE (A.GlobalReference x1) =
  [||L.GlobalReference <$> $$(qqExpM x1)||]
qqConstantE (A.AntiConstant s) =
  unsafeTExpCoerce [|$(antiVarE s) >>= (return . toConstant)|]

qqNameE :: Conversion A.Name L.Name
qqNameE (A.Name x1) =
  [||L.Name <$> $$(qqExpM x1)||]
qqNameE (A.UnName x1) =
  [||L.UnName <$> $$(qqExpM x1)||]
qqNameE A.NeedsName = do
  n <- runIO $ atomicModifyIORef' counter $ \n -> (n+1,n)
  [||pure $ L.Name $ "n" ++ show (n :: Int)||]
qqNameE (A.AntiName s) =
  unsafeTExpCoerce [|$(antiVarE s) >>= return . toName|]

qqFloatingPointFormatE :: Conversion A.FloatingPointFormat L.FloatingPointFormat
qqFloatingPointFormatE A.IEEE =
  [||pure L.IEEE||]
qqFloatingPointFormatE A.DoubleExtended =
  [||pure L.DoubleExtended||]
qqFloatingPointFormatE A.PairOfFloats =
  [||pure L.PairOfFloats||]

qqTypeE :: Conversion A.Type L.Type
qqTypeE A.VoidType =
  [||pure L.VoidType||]
qqTypeE (A.IntegerType x1) =
  [||L.IntegerType <$> $$(qqExpM x1)||]
qqTypeE (A.PointerType x1 x2) =
  [||L.PointerType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.FloatingPointType x1 x2) =
  [||L.FloatingPointType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.FunctionType x1 x2 x3) =
  [||L.FunctionType <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqTypeE (A.VectorType x1 x2) =
  [||L.VectorType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.StructureType x1 x2) =
  [||L.StructureType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.ArrayType x1 x2) =
  [||L.ArrayType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.NamedTypeReference x1) =
  [||L.NamedTypeReference <$> $$(qqExpM x1)||]
qqTypeE A.MetadataType =
  [||pure L.MetadataType||]
qqTypeE (A.AntiType s) =
  unsafeTExpCoerce $ antiVarE s

qqDialectE :: Conversion A.Dialect L.Dialect
qqDialectE A.ATTDialect =
  [||pure L.ATTDialect||]
qqDialectE A.IntelDialect =
  [||pure L.IntelDialect||]

qqInlineAssemblyE :: Conversion A.InlineAssembly L.InlineAssembly
qqInlineAssemblyE (A.InlineAssembly x1 x2 x3 x4 x5 x6) =
  [||L.InlineAssembly <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                      <*> $$(qqExpM x5) <*> $$(qqExpM x6)||]

qqMapE :: (QQExp [(a, b)] [(c, d)], Ord c) => Conversion (M.Map a b) (M.Map c d)
qqMapE m =
  [||M.fromList <$> $$(qqExpM (M.toList m))||]

qqSetE :: (QQExp [a] [b], Ord b) => Conversion (S.Set a) (S.Set b)
qqSetE m =
  [||S.fromList <$> $$(qqExpM (S.toList m))||]

qqEndiannessE :: Conversion A.Endianness L.Endianness
qqEndiannessE A.LittleEndian =
  [||pure L.LittleEndian||]
qqEndiannessE A.BigEndian =
  [||pure L.BigEndian||]

qqAlignmentInfoE :: Conversion A.AlignmentInfo L.AlignmentInfo
qqAlignmentInfoE (A.AlignmentInfo x1 x2) =
  [||L.AlignmentInfo <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]

qqAlignTypeE :: Conversion A.AlignType L.AlignType
qqAlignTypeE A.IntegerAlign =
  [||pure L.IntegerAlign||]
qqAlignTypeE A.VectorAlign =
  [||pure L.VectorAlign||]
qqAlignTypeE A.FloatAlign =
  [||pure L.FloatAlign||]
qqAlignTypeE A.AggregateAlign =
  [||pure L.AggregateAlign||]
qqAlignTypeE A.StackAlign =
  [||pure L.StackAlign||]

qqDataLayoutE :: Conversion A.DataLayout L.DataLayout
qqDataLayoutE (A.DataLayout x1 x2 x3 x4 x5) =
  [||L.DataLayout <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                  <*> $$(qqExpM x5)||]
qqDataLayoutE (A.AntiDataLayout s) =
  unsafeTExpCoerce $ antiVarE s

qqTargetTripleE :: Conversion A.TargetTriple (Maybe String)
qqTargetTripleE A.NoTargetTriple =
  [||pure Nothing||]
qqTargetTripleE (A.TargetTriple v) =
  [||Just <$> $$(qqExpM v)||]
qqTargetTripleE (A.AntiTargetTriple v) =
  unsafeTExpCoerce [|$(antiVarE v) >>= return . toTargetTriple|]

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
qqNamedP (A.AntiInstructionList s) =
  error $ "Antiquote not allowed in pattern: " ++ s

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
qqNameP A.NeedsName =
  error "can't use unnamed basic blocks within pattern quote"

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
qqPat = const Nothing
 `extQ` qqDefinitionP
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

quasiquoteM :: forall a b m. (Data a, QQExp a b, Monad m, Applicative m)
           => [A.Extensions]
           -> P.P a
           -> TQuasiQuoter (m b)
quasiquoteM exts p = TQuasiQuoter $
  QuasiQuoter { quoteExp  = parse exts p >=> unTypeQ . (qqExpM :: Conversion' m a b)
              , quotePat  = fail "LLVM monadic pattern quasiquoter undefined"
              , quoteType = fail "LLVM type quasiquoter undefined"
              , quoteDec  = fail "LLVM declaration quasiquoter undefined"
              }
