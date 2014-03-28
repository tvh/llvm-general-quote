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
  Name(..),
  FloatingPointFormat(..),
  Type(..),
  Dialect(..),
  InlineAssembly(..),
  Endianness(..),
  AlignmentInfo(..),
  AlignType(..),
  DataLayout(..),
  TargetTriple(..),
  Extensions(..), ExtensionsInt
  ) where

import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.Linkage as A
import qualified LLVM.General.AST.Visibility as A
import qualified LLVM.General.AST.CallingConvention as A
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.IntegerPredicate as AI
import qualified LLVM.General.AST.FloatingPointPredicate as AF
import qualified LLVM.General.AST.RMWOperation as A

import Data.Word
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH.Lift

data Extensions
  = Antiquotation
  | Loops
  deriving (Eq, Ord, Enum, Show)
type ExtensionsInt = Word32

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
data Global
    -- | <http://llvm.org/docs/LangRef.html#global-variables>
    = GlobalVariable {
        name :: Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        isThreadLocal :: Bool,
        addrSpace :: A.AddrSpace,
        hasUnnamedAddr :: Bool,
        isConstant :: Bool,
        _type' :: Type,
        initializer :: Maybe Constant,
        section :: Maybe String,
        alignmentG :: Word32
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        _type' :: Type,
        aliasee :: Constant
      }
    -- | <http://llvm.org/docs/LangRef.html#functions>
    | Function {
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        _callingConvention :: A.CallingConvention,
        _returnAttributes :: [A.ParameterAttribute],
        returnType :: Type,
        name :: Name,
        parameters :: ([Parameter],Bool), -- ^ snd indicates varargs
        _functionAttributes :: [A.FunctionAttribute],
        section :: Maybe String,
        alignment :: Word32,
        garbageCollectorName :: Maybe String,
        basicBlocks :: [BasicBlock]
      }
  deriving (Eq, Read, Show, Typeable, Data)

-- | 'Parameter's for 'Function's
data Parameter
  = Parameter Type Name [A.ParameterAttribute]
  | AntiParameter String
  | AntiParameterList String
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
-- LLVM code in a function is a sequence of 'BasicBlock's each with a label,
-- some instructions, and a terminator.
data BasicBlock
  = BasicBlock {
    label :: Name,
    instructions :: [Named Instruction],
    terminator :: (Named Terminator)
  }
  | ForLoop {
    label :: Name,
    iterType :: Type,
    iterName :: Name,
    from :: Operand,
    to :: Operand,
    _elementType :: Type,
    _element :: [(Operand,Name)],
    _elementName :: Name,
    body :: [BasicBlock],
    next :: Maybe Name}
  | AntiBasicBlock String
  | AntiBasicBlockList String
  deriving (Eq, Read, Show, Typeable, Data)

-- | Any thing which can be at the top level of a 'Module'
data Definition 
  = GlobalDefinition Global
  | TypeDefinition Name (Maybe Type)
  | MetadataNodeDefinition MetadataNodeID [Maybe Operand]
  | NamedMetadataDefinition String [MetadataNodeID]
  | ModuleInlineAssembly String
  | AntiDefinition String
  | AntiDefinitionList String
    deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#modulestructure>
data Module = 
  Module {
    moduleName :: String,
    -- | a 'DataLayout', if specified, must match that of the eventual code generator
    moduleDataLayout :: Maybe DataLayout, 
    moduleTargetTriple :: TargetTriple,
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
      trueDest :: Name, 
      falseDest :: Name,
      metadata' :: InstructionMetadata
    }
  | Br { 
      dest :: Name,
      metadata' :: InstructionMetadata
    }
  | Switch {
      operand0' :: Operand,
      defaultDest :: Name,
      dests :: [(Constant, Name)],
      metadata' :: InstructionMetadata
    }
  | IndirectBr {
      operand0' :: Operand,
      possibleDests :: [Name],
      metadata' :: InstructionMetadata
    }
  | Invoke {
      callingConvention' :: A.CallingConvention,
      returnAttributes' :: [A.ParameterAttribute],
      function' :: CallableOperand,
      arguments' :: [(Operand, [A.ParameterAttribute])],
      functionAttributes' :: [A.FunctionAttribute],
      returnDest :: Name,
      exceptionDest :: Name,
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
      allocatedType :: Type,
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
      type' :: Type,
      metadata :: InstructionMetadata 
    }
  | ZExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata 
    }
  | SExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPToUI {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPToSI {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | UIToFP {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | SIToFP {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPTrunc {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | PtrToInt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | IntToPtr {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | BitCast {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | AddrSpaceCast {
      operand0 :: Operand,
      type' :: Type,
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
      type' :: Type,
      incomingValues :: [ (Operand, Name) ],
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
      type' :: Type,
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
      type' :: Type,
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
  = Name := a
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
  = LocalReference Name
  -- | 'Constant's include 'LLVM.General.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataStringOperand String
  | MetadataNodeOperand MetadataNode
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | The 'LLVM.General.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either InlineAssembly Operand

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
    | Null { constantType :: Type }
    | Struct { structName :: Maybe Name, _isPacked :: Bool, memberValues :: [ Constant ] }
    | Array { memberType :: Type, memberValues :: [ Constant ] }
    | Vector { memberValues :: [ Constant ] }
    | Undef { constantType :: Type }
    | BlockAddress { blockAddressFunction :: Name, blockAddressBlock :: Name }
    | GlobalReference Name
    | AntiConstant String
    deriving (Eq, Ord, Read, Show, Typeable, Data)

{- |
Objects of various sorts in LLVM IR are identified by address in the LLVM C++ API, and
may be given a string name. When printed to (resp. read from) human-readable LLVM assembly, objects without
string names are numbered sequentially (resp. must be numbered sequentially). String names may be quoted, and
are quoted when printed if they would otherwise be misread - e.g. when containing special characters. 

> 7

means the seventh unnamed object, while

> "7"

means the object named with the string "7".

This libraries handling of 'UnName's during translation of the AST down into C++ IR is somewhat more
forgiving than the LLVM assembly parser: it does not require that unnamed values be numbered sequentially;
however, the numbers of 'UnName's passed into C++ cannot be preserved in the C++ objects. If the C++ IR is
printed as assembly or translated into a Haskell AST, unnamed nodes will be renumbered sequentially. Thus
unnamed node numbers should be thought of as having any scope limited to the 'LLVM.General.AST.Module' in
which they are used.
-}
data Name 
    = Name String -- ^ a string name 
    | UnName Word -- ^ a number for a nameless thing
    | AntiName String
   deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | LLVM supports some special formats floating point format. This type is to distinguish those format.
-- I believe it's treated as a format for "a" float, as opposed to a vector of two floats, because
-- its intended usage is to represent a single number with a combined significand.
data FloatingPointFormat
  = IEEE
  | DoubleExtended
  | PairOfFloats
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#type-system>
data Type
  -- | <http://llvm.org/docs/LangRef.html#void-type>
  = VoidType
  -- | <http://llvm.org/docs/LangRef.html#integer-type>
  | IntegerType { typeBits :: Word32 }
  -- | <http://llvm.org/docs/LangRef.html#pointer-type>
  | PointerType { pointerReferent :: Type, pointerAddrSpace :: A.AddrSpace }
  -- | <http://llvm.org/docs/LangRef.html#floating-point-types>
  | FloatingPointType { typeBits :: Word32, floatingPointFormat :: FloatingPointFormat }
  -- | <http://llvm.org/docs/LangRef.html#function-type>
  | FunctionType { resultType :: Type, argumentTypes :: [Type], isVarArg :: Bool }
  -- | <http://llvm.org/docs/LangRef.html#vector-type>
  | VectorType { nVectorElements :: Word32, elementType :: Type }
  -- | <http://llvm.org/docs/LangRef.html#structure-type>
  | StructureType { isPacked :: Bool, elementTypes :: [Type] }
  -- | <http://llvm.org/docs/LangRef.html#array-type>
  | ArrayType { nArrayElements :: Word64, elementType :: Type }
  -- | <http://llvm.org/docs/LangRef.html#opaque-structure-types>
  | NamedTypeReference Name
  -- | <http://llvm.org/docs/LangRef.html#metadata-type>
  | MetadataType -- only to be used as a parameter type for a few intrinsics
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | the dialect of assembly used in an inline assembly string
-- <http://en.wikipedia.org/wiki/X86_assembly_language#Syntax>
data Dialect
  = ATTDialect
  | IntelDialect
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#inline-assembler-expressions>
-- to be used through 'LLVM.General.AST.Operand.CallableOperand' with a
-- 'LLVM.General.AST.Instruction.Call' instruction
data InlineAssembly
  = InlineAssembly {
      __type' :: Type,
      assembly :: String,
      constraints :: String,
      hasSideEffects :: Bool,
      alignStack :: Bool,
      dialect :: Dialect
    }
  deriving (Eq, Read, Show, Typeable, Data)

-- | Little Endian is the one true way :-). Sadly, we must support the infidels.
data Endianness = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | An AlignmentInfo describes how a given type must and would best be aligned
data AlignmentInfo = AlignmentInfo {
    abiAlignment :: Word32,
    preferredAlignment :: Maybe Word32
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A type of type for which 'AlignmentInfo' may be specified
data AlignType
  = IntegerAlign
  | VectorAlign
  | FloatAlign
  | AggregateAlign
  | StackAlign
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | a description of the various data layout properties which may be used during
-- optimization
data DataLayout
  = DataLayout {
    endianness :: Maybe Endianness,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: M.Map A.AddrSpace (Word32, AlignmentInfo),
    typeLayouts :: M.Map (AlignType, Word32) AlignmentInfo,
    nativeSizes :: Maybe (S.Set Word32)
  }
  | AntiDataLayout String
  deriving (Eq, Ord, Read, Show, Typeable, Data)

data TargetTriple
  = NoTargetTriple
  | TargetTriple String
  | AntiTargetTriple String
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
                  ''InlineAssembly,
                  ''Dialect,
                  ''A.RMWOperation,
                  ''Atomicity,
                  ''LandingPadClause,
                  ''MemoryOrdering,
                  ''Terminator,
                  ''Name,
                  ''MetadataNode,
                  ''MetadataNodeID,
                  ''Operand,
                  ''Type,
                  ''FloatingPointFormat,
                  ''DataLayout,
                  ''Endianness,
                  ''M.Map,
                  ''AlignType,
                  ''AlignmentInfo,
                  ''S.Set,
                  ''Definition,
                  ''Module,
                  ''TargetTriple
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