module Language.LLVM.Parser.Tokens (
    Token(..),
    Visibility(..),
    Extensions(..),
    ExtensionsInt,
    keywords,
    keywordMap
  ) where

import qualified Data.Map as Map
import Data.Bits
import Data.Word
import Text.PrettyPrint.Mainland
import Data.List (foldl')

data Visibility
  = Global
  | Local
  deriving (Eq, Ord, Show)

data Token
  = Teof
  | TintConst (String, Integer)
  | TfloatConst (String, Rational)
  | TstringConst (String, String)
  | Tnamed Visibility String
  | Tunamed Visibility Integer
  | Tlparen
  | Trparen
  | Tlbrack
  | Trbrack
  | Tlbrace
  | Trbrace
  | Tcomma
  | Tsemi
  | Tcolon
  | Tquestion
  | Tdot
  | Tassign

  | Tret
  | Tcondbr
  | Tbr
  | Tswitch
  | Tindirectbr
  | Tinvoke
  | Tresume
  | Tunreachable
  
  | Tadd
  | Tfadd
  | Tsub
  | Tfsub
  | Tmul
  | Tfmul
  | Tudiv
  | Tsdiv
  | Tfdiv
  | Turem
  | Tsrem
  | Tfrem
  | Tshl
  | Tlshr
  | Tashr
  | Tand
  | Txor
  | Talloca
  | Tload
  | Tstore
  | Tgetelementptr
  | Tfence
  | Tcmpxchg
  | Tatomicrmw
  | Ttrunc
  | Tzext
  | Tsext
  | Tfptoui
  | Tfttosi
  | Tuitofp
  | Tsitofp
  | Tfptrunc
  | Tfpext
  | Tptrtoint
  | Tinttoptr
  | Tbitcast
  | Taddrspacecast
  | Ticmp
  | Tfcmp
  | Tphi
  | Tcall
  | Tselect
  | Tvaarg
  | Textractelement
  | Tinsertelement
  | Tshufflevector
  | Textractvalue
  | Tinsertvalue
  | Tlandingpad

  | Ttarget
  | Tdatalayout
  | Ttriple
  | Tdefine

  | Tintegertype Integer
  deriving (Eq, Ord, Show)

instance Pretty Token where
    ppr = text . show
  
keywords :: [(String,      Token,      Maybe [Extensions])]
keywords = [("define",       Tdefine,      Nothing),
            ("ret",          Tret,         Nothing)
           ]

data Extensions = Antiquotation
  deriving (Eq, Ord, Enum, Show)
type ExtensionsInt = Word32

keywordMap :: Map.Map String (Token, Maybe ExtensionsInt)
keywordMap = Map.fromList (map f keywords)
  where
    f  ::  (String, Token, Maybe [Extensions])
       ->  (String, (Token, Maybe ExtensionsInt))
    f (s, t, Nothing)    = (s, (t, Nothing))
    f (s, t, Just exts)  = (s, (t, Just i))
      where
        i = foldl' setBit 0 (map fromEnum exts)