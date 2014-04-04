module LLVM.General.Quote.Parser (
    module P,
    parse
  ) where

import Control.Exception

import qualified Data.ByteString.Char8 as B
import Data.Loc

import LLVM.General.Quote.Parser.Lexer as P
import LLVM.General.Quote.Parser.Tokens as P
import LLVM.General.Quote.Parser.Monad as P

parse :: [Extensions]
      -> P a
      -> B.ByteString
      -> Pos
      -> Either SomeException a
parse exts p bs pos =
    evalP p (emptyPState exts bs pos)
