{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.LLVM.AST (
  Module(..),
  Definition(..),
  Global(..),
  Parameter(..),
  BasicBlock(..),
  InstructionMetadata,
  Terminator(..),
  MemoryOrdering(..),
  Atomicity(..),
  LandingPadClause(..),
  Instruction(..),
  Named(..),
  Extensions(..), ExtensionsInt
  ) where

import qualified LLVM.General.AST.Constant as A
  (Constant())
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.Operand as A
import qualified LLVM.General.AST.Name as A
import qualified LLVM.General.AST.Type as A
import qualified LLVM.General.AST.Linkage as A
import qualified LLVM.General.AST.Visibility as A
import qualified LLVM.General.AST.CallingConvention as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.IntegerPredicate as AI
import qualified LLVM.General.AST.FloatingPointPredicate as AF
import qualified LLVM.General.AST.InlineAssembly as A
import qualified LLVM.General.AST.RMWOperation as A

import Data.Word
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH.Lift

data Extensions = Antiquotation
  deriving (Eq, Ord, Enum, Show)
type ExtensionsInt = Word32

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
data Global
    -- | <http://llvm.org/docs/LangRef.html#global-variables>
    = GlobalVariable {
        name :: A.Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        isThreadLocal :: Bool,
        addrSpace :: A.AddrSpace,
        hasUnnamedAddr :: Bool,
        isConstant :: Bool,
        _type' :: A.Type,
        initializer :: Maybe A.Constant,
        section :: Maybe String,
        alignmentG :: Word32
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: A.Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        _type' :: A.Type,
        aliasee :: A.Constant
      }
    -- | <http://llvm.org/docs/LangRef.html#functions>
    | Function {
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        _callingConvention :: A.CallingConvention,
        _returnAttributes :: [A.ParameterAttribute],
        returnType :: A.Type,
        name :: A.Name,
        parameters :: ([Parameter],Bool), -- ^ snd indicates varargs
        _functionAttributes :: [A.FunctionAttribute],
        section :: Maybe String,
        alignment :: Word32,
        garbageCollectorName :: Maybe String,
        basicBlocks :: [BasicBlock]
      }
  deriving (Eq, Read, Show, Typeable, Data)

-- | 'Parameter's for 'Function's
data Parameter = Parameter A.Type A.Name [A.ParameterAttribute]
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
-- LLVM code in a function is a sequence of 'BasicBlock's each with a label,
-- some instructions, and a terminator.
data BasicBlock
  = BasicBlock A.Name [Named Instruction] (Named Terminator)
  | AntiBasicBlocks String
  deriving (Eq, Read, Show, Typeable, Data)

-- | Any thing which can be at the top level of a 'Module'
data Definition 
  = GlobalDefinition Global
  | TypeDefinition A.Name (Maybe A.Type)
  | MetadataNodeDefinition A.MetadataNodeID [Maybe A.Operand]
  | NamedMetadataDefinition String [A.MetadataNodeID]
  | ModuleInlineAssembly String
  | AntiDefinitionList String
    deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#modulestructure>
data Module = 
  Module {
    moduleName :: String,
    -- | a 'DataLayout', if specified, must match that of the eventual code generator
    moduleDataLayout :: Maybe A.DataLayout, 
    moduleTargetTriple :: Maybe String,
    moduleDefinitions :: [Definition]
  } 
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#metadata-nodes-and-metadata-strings>
-- Metadata can be attached to an instruction
type InstructionMetadata = [(String, A.MetadataNode)]

-- | <http://llvm.org/docs/LangRef.html#terminators>
data Terminator 
  = Ret { 
      returnOperand :: Maybe A.Operand,
      metadata' :: InstructionMetadata
    }
  | CondBr { 
      condition :: A.Operand, 
      trueDest :: A.Name, 
      falseDest :: A.Name,
      metadata' :: InstructionMetadata
    }
  | Br { 
      dest :: A.Name,
      metadata' :: InstructionMetadata
    }
  | Switch {
      operand0' :: A.Operand,
      defaultDest :: A.Name,
      dests :: [(A.Constant, A.Name)],
      metadata' :: InstructionMetadata
    }
  | IndirectBr {
      operand0' :: A.Operand,
      possibleDests :: [A.Name],
      metadata' :: InstructionMetadata
    }
  | Invoke {
      callingConvention' :: A.CallingConvention,
      returnAttributes' :: [A.ParameterAttribute],
      function' :: A.CallableOperand,
      arguments' :: [(A.Operand, [A.ParameterAttribute])],
      functionAttributes' :: [A.FunctionAttribute],
      returnDest :: A.Name,
      exceptionDest :: A.Name,
      metadata' :: InstructionMetadata
    }
  | Resume {
      operand0' :: A.Operand,
      metadata' :: InstructionMetadata
    }
  | Unreachable {
      metadata' :: InstructionMetadata
    }
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#atomic-memory-ordering-constraints>
-- <http://llvm.org/docs/Atomics.html>
data MemoryOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcquireRelease
  | SequentiallyConsistent
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | An 'Atomicity' describes constraints on the visibility of effects of an atomic instruction
data Atomicity = Atomicity { 
  crossThread :: Bool, -- ^ <http://llvm.org/docs/LangRef.html#singlethread>
  memoryOrdering :: MemoryOrdering
 }
 deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | For the redoubtably complex 'LandingPad' instruction
data LandingPadClause
    = Catch A.Constant
    | Filter A.Constant
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | non-terminator instructions:
-- <http://llvm.org/docs/LangRef.html#binaryops>
-- <http://llvm.org/docs/LangRef.html#bitwiseops>
-- <http://llvm.org/docs/LangRef.html#memoryops>
-- <http://llvm.org/docs/LangRef.html#otherops>
data Instruction
  = Add { 
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: A.Operand,
      operand1 :: A.Operand,
      metadata :: InstructionMetadata
    }
  | FAdd {
      operand0 :: A.Operand,
      operand1 :: A.Operand,
      metadata :: InstructionMetadata
    }
  | Sub {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: A.Operand,
      operand1 :: A.Operand,
      metadata :: InstructionMetadata
    }
  | FSub { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | Mul { 
      nsw :: Bool, 
      nuw :: Bool, 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata 
    }
  | FMul { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | UDiv { 
      exact :: Bool, 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | SDiv { 
      exact :: Bool, 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | FDiv { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | URem { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | SRem { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | FRem { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | Shl { 
      nsw :: Bool, 
      nuw :: Bool, 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | LShr { 
      exact :: Bool, 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | AShr { 
      exact :: Bool, 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | And { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | Or { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | Xor { 
      operand0 :: A.Operand, 
      operand1 :: A.Operand, 
      metadata :: InstructionMetadata
    }
  | Alloca { 
      allocatedType :: A.Type,
      numElements :: Maybe A.Operand,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | Load {
      volatile :: Bool, 
      address :: A.Operand,
      maybeAtomicity :: Maybe Atomicity,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | Store {
      volatile :: Bool, 
      address :: A.Operand,
      value :: A.Operand,
      maybeAtomicity :: Maybe Atomicity,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | GetElementPtr { 
      inBounds :: Bool,
      address :: A.Operand,
      indices :: [A.Operand],
      metadata :: InstructionMetadata
    }
  | Fence { 
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | CmpXchg { 
      volatile :: Bool,
      address :: A.Operand,
      expected :: A.Operand,
      replacement :: A.Operand,
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | AtomicRMW { 
      volatile :: Bool,
      rmwOperation :: A.RMWOperation,
      address :: A.Operand,
      value :: A.Operand,
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | Trunc { 
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata 
    }
  | ZExt {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata 
    }
  | SExt {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPToUI {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPToSI {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | UIToFP {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | SIToFP {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPTrunc {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPExt {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | PtrToInt {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | IntToPtr {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | BitCast {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | AddrSpaceCast {
      operand0 :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | ICmp {
      iPredicate :: AI.IntegerPredicate,
      operand0 :: A.Operand,
      operand1 :: A.Operand,
      metadata :: InstructionMetadata
    }
  | FCmp {
      fpPredicate :: AF.FloatingPointPredicate,
      operand0 :: A.Operand,
      operand1 :: A.Operand,
      metadata :: InstructionMetadata
    }
  | Phi {
      type' :: A.Type,
      incomingValues :: [ (A.Operand, A.Name) ],
      metadata :: InstructionMetadata
  } 
  | Call {
      isTailCall :: Bool,
      callingConvention :: A.CallingConvention,
      returnAttributes :: [A.ParameterAttribute],
      function :: A.CallableOperand,
      arguments :: [(A.Operand, [A.ParameterAttribute])],
      functionAttributes :: [A.FunctionAttribute],
      metadata :: InstructionMetadata
  }
  | Select { 
      condition' :: A.Operand,
      trueValue :: A.Operand,
      falseValue :: A.Operand,
      metadata :: InstructionMetadata
    }
  | VAArg { 
      argList :: A.Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata 
    }
  | ExtractElement { 
      vector :: A.Operand,
      index :: A.Operand,
      metadata :: InstructionMetadata 
    }
  | InsertElement { 
      vector :: A.Operand,
      element :: A.Operand,
      index :: A.Operand,
      metadata :: InstructionMetadata
    }
  | ShuffleVector { 
      operand0 :: A.Operand,
      operand1 :: A.Operand,
      mask :: A.Constant,
      metadata :: InstructionMetadata
    }
  | ExtractValue { 
      aggregate :: A.Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | InsertValue { 
      aggregate :: A.Operand,
      element :: A.Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | LandingPad { 
      type' :: A.Type,
      personalityFunction :: A.Operand,
      cleanup :: Bool,
      clauses :: [LandingPadClause],
      metadata :: InstructionMetadata
    }
  | AntiInstruction String
  deriving (Eq, Read, Show, Typeable, Data)

-- | Instances of instructions may be given a name, allowing their results to be referenced as 'Operand's.
-- Sometimes instructions - e.g. a call to a function returning void - don't need names.
data Named a 
  = A.Name := a
  | Do a
  deriving (Eq, Read, Show, Typeable, Data)


$(deriveLiftMany [''A.Visibility,
                  ''A.Linkage,
                  ''A.ParameterAttribute,
                  ''Global,
                  ''A.Constant,
                  ''A.AddrSpace,
                  ''A.CallingConvention,
                  ''A.FunctionAttribute,
                  ''A.SomeFloat,
                  ''AI.IntegerPredicate,
                  ''AF.FloatingPointPredicate,
                  ''BasicBlock,
                  ''Parameter,
                  ''Named,
                  ''Instruction,
                  ''A.InlineAssembly,
                  ''A.Dialect,
                  ''A.RMWOperation,
                  ''Atomicity,
                  ''LandingPadClause,
                  ''MemoryOrdering,
                  ''Terminator,
                  ''A.Name,
                  ''A.MetadataNode,
                  ''A.MetadataNodeID,
                  ''A.Operand,
                  ''A.Type,
                  ''A.FloatingPointFormat,
                  ''A.DataLayout,
                  ''A.Endianness,
                  ''M.Map,
                  ''A.AlignType,
                  ''A.AlignmentInfo,
                  ''S.Set,
                  ''Definition,
                  ''Module
                  ])
instance Lift Word64 where
  lift = lift . toInteger
instance Lift Word32 where
  lift = lift . toInteger
instance Lift Word16 where
  lift = lift . toInteger
instance Lift Word where
  lift = lift . toInteger
instance Lift Float where
  lift = lift . toRational
instance Lift Double where
  lift = lift . toRational