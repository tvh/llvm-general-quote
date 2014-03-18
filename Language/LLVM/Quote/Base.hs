{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module Language.LLVM.Quote.Base (
    ToDefintion(..),
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

class ToDefintion a where
  toDefinition :: a -> L.Definition
instance ToDefintion L.Definition where
  toDefinition = id
instance ToDefintion L.Global where
  toDefinition = L.GlobalDefinition

antiVarE :: String -> ExpQ
antiVarE = either fail return . parseExp

qqDefinitionListE :: [A.Definition] -> Maybe (Q Exp)
qqDefinitionListE [] = Just [|[]|]
qqDefinitionListE (A.AntiDefinitionList v : defs) =
    Just [|map toDefinition $(antiVarE v) ++ $(qqE defs)|]
qqDefinitionListE (def : defs) =
    Just [|$(qqE def) : $(qqE defs)|]

qqDefinitionE :: A.Definition -> Maybe (Q Exp)
qqDefinitionE (A.GlobalDefinition v) =
    Just [|L.GlobalDefinition v :: L.Definition|]
qqDefinitionE (A.TypeDefinition n v) =
    Just [|L.TypeDefinition n v :: L.Definition|]
qqDefinitionE (A.MetadataNodeDefinition i vs) =
    Just [|L.MetadataNodeDefinition i vs :: L.Definition|]
qqDefinitionE (A.NamedMetadataDefinition i vs) =
    Just [|L.NamedMetadataDefinition i vs :: L.Definition|]
qqDefinitionE (A.ModuleInlineAssembly s) =
    Just [|L.ModuleInlineAssembly s :: L.Definition|]
qqDefinitionE (A.AntiDefinition v) =
    Just [|toDefinition $(antiVarE v) :: L.Definition|]
qqDefinitionE x = error $ "QuasiQuoting not Possible: " ++ show x

qqModuleE :: A.Module -> Maybe (Q Exp)
qqModuleE (A.Module n dl tt ds) = 
  Just [|L.Module $(qqE n) $(qqE dl) $(qqE tt) $(qqE ds) :: L.Module|]

qqE x = dataToExpQ qqExp x

qqExp :: Typeable a => a -> Maybe (Q Exp)
qqExp = const Nothing `extQ` qqDefinitionE
                      `extQ` qqDefinitionListE
                      `extQ` qqModuleE

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