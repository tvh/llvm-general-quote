{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module Language.LLVM.Quote.Base (
    ToDefintions(..),
    quasiquote
  ) where

import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as B
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Loc
import Data.Typeable (Typeable(..))
import Language.Haskell.Meta (parseExp, parsePat)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToQa,
                                  dataToExpQ,
                                  dataToPatQ)
import Language.Haskell.TH.Syntax

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

qqE x = dataToExpQ qqExp x

qqExp :: Typeable a => a -> Maybe (Q Exp)
qqExp = const Nothing `extQ` qqDefinitionE
                      `extQ` qqDefinitionListE
                      `extQ` qqModuleE
                      `extQ` qqGlobalE
                      `extQ` qqParameterE
                      `extQ` qqBasicBlockE
                      `extQ` qqBasicBlockListE

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