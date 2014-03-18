{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.LLVM.AST (
  Module(..),
  Definition(..),
  Extensions(..), ExtensionsInt
  ) where

import qualified LLVM.General.AST.Constant as A
  (Constant())
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.Instruction as A
import qualified LLVM.General.AST.Operand as A
import qualified LLVM.General.AST.Name as A
import qualified LLVM.General.AST.Global as A
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

-- | Any thing which can be at the top level of a 'Module'
data Definition 
  = GlobalDefinition A.Global
  | TypeDefinition A.Name (Maybe A.Type)
  | MetadataNodeDefinition A.MetadataNodeID [Maybe A.Operand]
  | NamedMetadataDefinition String [A.MetadataNodeID]
  | ModuleInlineAssembly String
  | AntiDefinition String
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
                  ''A.Global,
                  ''A.Constant,
                  ''A.AddrSpace,
                  ''A.CallingConvention,
                  ''A.FunctionAttribute,
                  ''A.SomeFloat,
                  ''AI.IntegerPredicate,
                  ''AF.FloatingPointPredicate,
                  ''A.BasicBlock,
                  ''A.Parameter,
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