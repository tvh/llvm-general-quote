module Language.LLVM.Parser (
    module Language.LLVM.Parser.Lexer,
    module Language.LLVM.Parser.Monad,
    module Language.LLVM.Parser.Parser,
    parse
  ) where

import Control.Exception

import qualified Data.ByteString.Char8 as B
import Data.Loc

import Language.LLVM.Parser.Lexer
import Language.LLVM.Parser.Parser
import Language.LLVM.Parser.Monad
import Language.LLVM.AST

parse :: [Extensions]
      -> P a
      -> B.ByteString
      -> Pos
      -> Either SomeException a
parse exts p bs pos =
    evalP p (emptyPState exts bs pos)
