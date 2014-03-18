{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.LLVM.AST (
  Module(..),
  Definition(..),
  Global(..),
  Parameter(..),
  BasicBlock(..),
  Extensions(..), ExtensionsInt
  ) where

import qualified LLVM.General.AST.Constant as A
  (Constant())
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.Instruction as A
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
        type' :: A.Type,
        initializer :: Maybe A.Constant,
        section :: Maybe String,
        alignment :: Word32
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: A.Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        type' :: A.Type,
        aliasee :: A.Constant
      }
    -- | <http://llvm.org/docs/LangRef.html#functions>
    | Function {
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        callingConvention :: A.CallingConvention,
        returnAttributes :: [A.ParameterAttribute],
        returnType :: A.Type,
        name :: A.Name,
        parameters :: ([Parameter],Bool), -- ^ snd indicates varargs
        functionAttributes :: [A.FunctionAttribute],
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
  = BasicBlock A.Name [A.Named A.Instruction] (A.Named A.Terminator)
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
                  ''A.Named,
                  ''A.Instruction,
                  ''A.InlineAssembly,
                  ''A.Dialect,
                  ''A.RMWOperation,
                  ''A.Atomicity,
                  ''A.LandingPadClause,
                  ''A.MemoryOrdering,
                  ''A.Terminator,
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