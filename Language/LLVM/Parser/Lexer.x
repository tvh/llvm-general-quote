{
-- |
-- Module      :  Language.LLVM.Parser.Lexer
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
--                (c) Drexel University 2013
--                (c) Timo von Holtz 2014
-- License     :  BSD-style
-- Maintainer  :  tvh@tvholtz.de

module Language.LLVM.Parser.Lexer (
    lexToken
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Char (isDigit,
                  isOctDigit,
                  isHexDigit,
                  chr,
                  toLower)
import Data.Loc
import Data.Ratio ((%))
import Text.PrettyPrint.Mainland

import Language.LLVM.Parser.Tokens
import Language.LLVM.Parser.Monad
}

$nondigit         = [a-z A-Z \_ \.]
$digit            = [0-9]
$nonzerodigit     = [1-9]
$octalDigit       = [0-7]
$hexadecimalDigit = [0-9A-Fa-f]
$whitechar = [\ \t\n\r\f\v]

@fractionalConstant = $digit* "." $digit+
                    | $digit+ "."
@exponentPart       = [eE] [\+\-]? $digit+

@floatingSuffix     = [fF]
                    | [lL]

@floatingConstant   = @fractionalConstant @exponentPart? @floatingSuffix?
                    | $digit+ @exponentPart @floatingSuffix?

@decimalConstant     = $nonzerodigit $digit* | "0"
@octalConstant       = "0" $octalDigit*
@hexadecimalConstant = "0" [xX] $hexadecimalDigit+

@integerSuffix = [uU] [lL]?
               | [lL] [uU]?
               | [lL] [lL] [uU]?
               | [uU] [lL] [lL]

@idText = [a-z A-Z \$ \. \_] [a-z A-Z \$ \. \_ 0-9]*
@identifier = [@\%] ( @decimalConstant
                    | @idText)
@jumpLabel = @idText ":"

@attrGroupNumber = "#" @decimalConstant

@integerType = "i" $nonzerodigit $digit*
@keyword = [a-z]+ ($nonzerodigit $digit*)?

@metaDataName = "!" ( @decimalConstant
                    | @idText)

tokens :-
<0> {
 ";" .* ;
 $whitechar+          ; 

 @identifier { identifier }
 @jumpLabel { jumpLabel }
 @integerType { numberedToken TintegerType }
 @keyword { keyword }
 @attrGroupNumber { numberedToken TattrGroupNumber }
 @metaDataName { metaDataName }

 @floatingConstant                    { lexFloat }
 @decimalConstant @integerSuffix?     { lexInteger 0 decimal }
 @octalConstant @integerSuffix?       { lexInteger 1 octal }
 @hexadecimalConstant @integerSuffix? { lexInteger 2 hexadecimal }

 \" { lexStringTok }

 "("   { token Tlparen }
 ")"   { token Trparen }
 "["   { token Tlbrack }
 "]"   { token Trbrack }
 "{"   { token Tlbrace }
 "}"   { token Trbrace }
 "<"   { token Tlt }
 ">"   { token Tgt }
 ","   { token Tcomma }
 ":"   { token Tcolon }
 "*"   { token Tstar }
 "="   { token Tassign }
 "-"   { token Tminus }
 "!"   { token Tbang }
}

{
type Action = AlexInput -> AlexInput -> P (L Token)

inputString :: AlexInput -> AlexInput -> String
inputString beg end =
  (B.unpack . B.take (alexOff end - alexOff beg)) (alexInput beg)

locateTok :: AlexInput -> AlexInput -> Token -> L Token
locateTok beg end tok =
    L (Loc (alexPos beg) (alexPos end)) tok

token :: Token -> Action
token tok beg end =
    return $ locateTok beg end tok

identifier :: Action
identifier beg end = do
    v <- case head ident of
      '%' -> return Local
      '@' -> return Global
    case isDigit $ head $ tail ident of
      False -> return $ locateTok beg end $ Tnamed v (tail ident)
      True  -> return $ locateTok beg end $ Tunnamed v (read $ tail ident)
  where
    ident :: String
    ident = inputString beg end

jumpLabel :: Action
jumpLabel beg end = do
    token (TjumpLabel $ init ident) beg end
  where
    ident :: String
    ident = inputString beg end

metaDataName :: Action
metaDataName beg end = do
    case isDigit $ head $ tail ident of
      False -> return $ locateTok beg end $ TmetaDataName (tail ident)
      True  -> return $ locateTok beg end $ TmetaDataNumber (read $ tail ident)
  where
    ident :: String
    ident = inputString beg end

numberedToken :: (Num a, Read a) => (a -> Token) -> Action
numberedToken f beg end = do
    return $ locateTok beg end $ f (read $ tail ident)
  where
    ident :: String
    ident = inputString beg end

keyword :: Action
keyword beg end = do
    case Map.lookup ident keywordMap of
      Nothing             -> identError
      Just (tok, Nothing) -> token tok beg end
      Just (tok, Just i)  -> do isKw <- useExts i
                                if isKw then token tok beg end else identError
  where
    ident :: String
    ident = inputString beg end

    identError = fail $ "not a valid keyword: " ++ show ident

lexStringTok :: Action
lexStringTok beg _ = do
    s    <- lexString ""
    end  <- getInput
    return $ locateTok beg end (TstringConst s)
  where
    lexString :: String -> P String
    lexString s = do
        c <- nextChar
        case c of
          '"'  -> return (reverse s)
          '\\' -> do  c' <- lexCharEscape
                      lexString (c' : s)
          _    -> lexString (c : s)

lexCharEscape :: P Char
lexCharEscape = do
    cur  <- getInput
    c    <- nextChar
    case c of
      'a'  -> return '\a'
      'b'  -> return '\b'
      'f'  -> return '\f'
      'n'  -> return '\n'
      'r'  -> return '\r'
      't'  -> return '\t'
      'v'  -> return '\v'
      '\\' -> return '\\'
      '\'' -> return '\''
      '"'  -> return '"'
      '?'  -> return '?'
      'x'  -> chr <$> checkedReadNum isHexDigit 16 hexDigit
      n | isOctDigit n -> setInput cur >> chr <$> checkedReadNum isOctDigit 8 octDigit
      c -> return c

lexInteger :: Int -> Radix -> Action
lexInteger ndrop radix@(_, isRadixDigit, _) beg end =
    case i of
      [n] -> return $ locateTok beg end (toToken n)
      _   -> fail "bad parse for integer"
  where
    num :: String
    num = (takeWhile isRadixDigit . drop ndrop)  s

    suffix :: String
    suffix = (map toLower . takeWhile (not . isRadixDigit) . reverse) s

    s :: String
    s = inputString beg end

    i :: [Integer]
    i = do  (n, _) <- readInteger radix num
            return n

    toToken :: Integer -> Token
    toToken n = TintConst n

lexFloat :: Action
lexFloat beg end =
    case i of
      [n] -> token (toToken n) beg end
      _   -> fail "bad parse for integer"
  where
    s :: String
    s = inputString beg end

    prefix :: String
    prefix = takeWhile (not . isSuffix) s

    suffix :: String
    suffix = (map toLower . takeWhile isSuffix . reverse) s

    isSuffix :: Char -> Bool
    isSuffix = (`elem` ['l', 'L', 'f', 'F'])

    i :: [Rational]
    i = do  (n, _) <- readRational s
            return n

    toToken :: Rational -> Token
    toToken n =
        case suffix of
          ""  -> TfloatConst n

type Radix = (Integer, Char -> Bool, Char -> Int)

decDigit :: Char -> Int
decDigit c  | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in decimal constant"

octDigit :: Char -> Int
octDigit c  | c >= '0' && c <= '7' = ord c - ord '0'
            | otherwise            = error "error in octal constant"

hexDigit :: Char -> Int
hexDigit c  | c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
            | c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
            | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in hexadecimal constant"

decimal :: Radix
decimal = (10, isDigit, decDigit)

octal :: Radix
octal = (8, isOctDigit, octDigit)

hexadecimal :: Radix
hexadecimal = (16, isHexDigit, hexDigit)

readInteger :: Radix -> ReadS Integer
readInteger (radix, isRadixDigit, charToInt) =
    go 0
  where
    go :: Integer -> ReadS Integer
    go  x  []             = return (x, "")
    go  x  (c : cs)
        | isRadixDigit c  = go (x * radix + toInteger (charToInt c)) cs
        | otherwise       = return (x, c : cs)

readDecimal :: ReadS Integer
readDecimal = readInteger decimal

readRational :: ReadS Rational
readRational s = do
    (n, d, t)  <- readFix s
    (x, _)     <- readExponent t
    return ((n % 1) * 10^^(x - toInteger d), t)
  where
    readFix :: String ->  [(Integer, Int, String)]
    readFix s =
        return (read (i ++ f), length f, u)
      where
        (i, t) = span isDigit s
        (f, u) = case t of
                   '.' : u  -> span isDigit u
                   _        -> ("", t)

    readExponent :: ReadS Integer
    readExponent ""                        = return (0, "")
    readExponent (e : s)  | e `elem` "eE"  = go s
                          | otherwise      = return (0, s)
      where
        go :: ReadS Integer
        go  ('+' : s)  = readDecimal s
        go  ('-' : s)  = do  (x, t) <- readDecimal s
                             return (-x, t)
        go  s          = readDecimal s

checkedReadNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
checkedReadNum isDigit base conv = do
    cur  <- getInput
    c    <- peekChar
    when (not $ isDigit c) $
       illegalNumericalLiteral cur
    readNum isDigit base conv

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
readNum isDigit base conv =
    read 0
  where
    read :: Int -> P Int
    read n = do
        c <- peekChar
        if isDigit c
          then do  let n' = n*base + conv c
                   n' `seq` skipChar >> read n'
          else return n

lexToken :: P (L Token)
lexToken = do
    beg  <- getInput
    sc   <- getLexState
    st   <- get
    case alexScanUser st beg sc of
      AlexEOF              -> token Teof beg beg
      AlexError end        -> lexerError end (text rest)
                                where
                                  rest :: String
                                  rest = B.unpack $ B.take 80 (alexInput end)
      AlexSkip end _       -> setInput end >> lexToken
      AlexToken end len t  -> setInput end >> t beg end

alexScanTokens :: P [L Token]
alexScanTokens = do
    beg  <- getInput
    sc   <- getLexState
    st   <- get
    case alexScanUser st beg sc of
      AlexEOF              -> return $ [locateTok beg beg Teof]
      AlexError end        -> lexerError end (text rest)
                                where
                                  rest :: String
                                  rest = B.unpack $ B.take 80 (alexInput end)
      AlexSkip end _       -> setInput end >> alexScanTokens
      AlexToken end len t  -> do
        setInput end
        token <- t beg end
        tokens <- alexScanTokens
        return $ token:tokens

scanTokens :: String -> Either SomeException [L Token]
scanTokens s' = 
  let s = B.pack s'
  in evalP alexScanTokens (emptyPState [] s (startPos s'))
}
  