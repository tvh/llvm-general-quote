module LLVM.General.Quote.Parser (
    module LLVM.General.Quote.Parser.Lexer,
    module LLVM.General.Quote.Parser.Tokens,
    module LLVM.General.Quote.Parser.Monad,
    parse
  ) where

import Control.Exception

import qualified Data.ByteString.Char8 as B
import Data.Loc

import LLVM.General.Quote.Parser.Lexer
import LLVM.General.Quote.Parser.Tokens
import LLVM.General.Quote.Parser.Monad

parse :: [Extensions]
      -> P a
      -> B.ByteString
      -> Pos
      -> Either SomeException a
parse exts p bs pos =
    evalP p (emptyPState exts bs pos)
