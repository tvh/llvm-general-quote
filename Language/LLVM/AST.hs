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
  MetadataNodeID(..),
  MetadataNode(..),
  Operand(..),
  CallableOperand,
  Constant(..),
  Extensions(..), ExtensionsInt
  ) where

import qualified LLVM.General.AST.Float as A
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
        initializer :: Maybe Constant,
        section :: Maybe String,
        alignmentG :: Word32
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: A.Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        _type' :: A.Type,
        aliasee :: Constant
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
  | MetadataNodeDefinition MetadataNodeID [Maybe Operand]
  | NamedMetadataDefinition String [MetadataNodeID]
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
type InstructionMetadata = [(String, MetadataNode)]

-- | <http://llvm.org/docs/LangRef.html#terminators>
data Terminator 
  = Ret { 
      returnOperand :: Maybe Operand,
      metadata' :: InstructionMetadata
    }
  | CondBr { 
      condition :: Operand, 
      trueDest :: A.Name, 
      falseDest :: A.Name,
      metadata' :: InstructionMetadata
    }
  | Br { 
      dest :: A.Name,
      metadata' :: InstructionMetadata
    }
  | Switch {
      operand0' :: Operand,
      defaultDest :: A.Name,
      dests :: [(Constant, A.Name)],
      metadata' :: InstructionMetadata
    }
  | IndirectBr {
      operand0' :: Operand,
      possibleDests :: [A.Name],
      metadata' :: InstructionMetadata
    }
  | Invoke {
      callingConvention' :: A.CallingConvention,
      returnAttributes' :: [A.ParameterAttribute],
      function' :: CallableOperand,
      arguments' :: [(Operand, [A.ParameterAttribute])],
      functionAttributes' :: [A.FunctionAttribute],
      returnDest :: A.Name,
      exceptionDest :: A.Name,
      metadata' :: InstructionMetadata
    }
  | Resume {
      operand0' :: Operand,
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
    = Catch Constant
    | Filter Constant
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
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FAdd {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Sub {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FSub { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Mul { 
      nsw :: Bool, 
      nuw :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata 
    }
  | FMul { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | UDiv { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | SDiv { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | FDiv { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | URem { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | SRem { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | FRem { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Shl { 
      nsw :: Bool, 
      nuw :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | LShr { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | AShr { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | And { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Or { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Xor { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Alloca { 
      allocatedType :: A.Type,
      numElements :: Maybe Operand,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | Load {
      volatile :: Bool, 
      address :: Operand,
      maybeAtomicity :: Maybe Atomicity,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | Store {
      volatile :: Bool, 
      address :: Operand,
      value :: Operand,
      maybeAtomicity :: Maybe Atomicity,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | GetElementPtr { 
      inBounds :: Bool,
      address :: Operand,
      indices :: [Operand],
      metadata :: InstructionMetadata
    }
  | Fence { 
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | CmpXchg { 
      volatile :: Bool,
      address :: Operand,
      expected :: Operand,
      replacement :: Operand,
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | AtomicRMW { 
      volatile :: Bool,
      rmwOperation :: A.RMWOperation,
      address :: Operand,
      value :: Operand,
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | Trunc { 
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata 
    }
  | ZExt {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata 
    }
  | SExt {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPToUI {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPToSI {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | UIToFP {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | SIToFP {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPTrunc {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | FPExt {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | PtrToInt {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | IntToPtr {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | BitCast {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | AddrSpaceCast {
      operand0 :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata
    }
  | ICmp {
      iPredicate :: AI.IntegerPredicate,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FCmp {
      fpPredicate :: AF.FloatingPointPredicate,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Phi {
      type' :: A.Type,
      incomingValues :: [ (Operand, A.Name) ],
      metadata :: InstructionMetadata
  } 
  | Call {
      isTailCall :: Bool,
      callingConvention :: A.CallingConvention,
      returnAttributes :: [A.ParameterAttribute],
      function :: CallableOperand,
      arguments :: [(Operand, [A.ParameterAttribute])],
      functionAttributes :: [A.FunctionAttribute],
      metadata :: InstructionMetadata
  }
  | Select { 
      condition' :: Operand,
      trueValue :: Operand,
      falseValue :: Operand,
      metadata :: InstructionMetadata
    }
  | VAArg { 
      argList :: Operand,
      type' :: A.Type,
      metadata :: InstructionMetadata 
    }
  | ExtractElement { 
      vector :: Operand,
      index :: Operand,
      metadata :: InstructionMetadata 
    }
  | InsertElement { 
      vector :: Operand,
      element :: Operand,
      index :: Operand,
      metadata :: InstructionMetadata
    }
  | ShuffleVector { 
      operand0 :: Operand,
      operand1 :: Operand,
      mask :: Constant,
      metadata :: InstructionMetadata
    }
  | ExtractValue { 
      aggregate :: Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | InsertValue { 
      aggregate :: Operand,
      element :: Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | LandingPad { 
      type' :: A.Type,
      personalityFunction :: Operand,
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

-- | A 'MetadataNodeID' is a number for identifying a metadata node.
-- Note this is different from "named metadata", which are represented with
-- 'LLVM.General.AST.NamedMetadataDefinition'.
newtype MetadataNodeID = MetadataNodeID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data MetadataNode 
  = MetadataNode [Maybe Operand]
  | MetadataNodeReference MetadataNodeID
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | An 'Operand' is roughly that which is an argument to an 'LLVM.General.AST.Instruction.Instruction'
data Operand 
  -- | %foo
  = LocalReference A.Name
  -- | 'Constant's include 'LLVM.General.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataStringOperand String
  | MetadataNodeOperand MetadataNode
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | The 'LLVM.General.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either A.InlineAssembly Operand

{- |
<http://llvm.org/docs/LangRef.html#constants>

N.B. - <http://llvm.org/docs/LangRef.html#constant-expressions>

Although constant expressions and instructions have many similarites, there are important
differences - so they're represented using different types in this AST. At the cost of making it
harder to move an code back and forth between being constant and not, this approach embeds more of
the rules of what IR is legal into the Haskell types.
-} 
data Constant
    = Int { integerBits :: Word32, integerValue :: Integer }
    | Float { floatValue :: A.SomeFloat }
    | Null { constantType :: A.Type }
    | Struct { structName :: Maybe A.Name, isPacked :: Bool, memberValues :: [ Constant ] }
    | Array { memberType :: A.Type, memberValues :: [ Constant ] }
    | Vector { memberValues :: [ Constant ] }
    | Undef { constantType :: A.Type }
    | BlockAddress { blockAddressFunction :: A.Name, blockAddressBlock :: A.Name }
    | GlobalReference A.Name
    | AntiConstant String
    deriving (Eq, Ord, Read, Show, Typeable, Data)



$(deriveLiftMany [''A.Visibility,
                  ''A.Linkage,
                  ''A.ParameterAttribute,
                  ''Global,
                  ''Constant,
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
                  ''MetadataNode,
                  ''MetadataNodeID,
                  ''Operand,
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