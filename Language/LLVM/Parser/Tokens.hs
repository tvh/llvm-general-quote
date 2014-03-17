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
import Language.LLVM.AST
import Data.List (foldl')

data Visibility
  = Global
  | Local
  deriving (Eq, Ord, Show)

data Token
  = Teof
  | TintConst Integer
  | TfloatConst Rational
  | TstringConst String
  | Tnamed Visibility String
  | Tunnamed Visibility Word
  | TjumpLabel String
  | TattrGroupNumber Integer
  | TmetaDataName String
  | TmetaDataNumber Word
  | Tlparen
  | Trparen
  | Tlbrack
  | Trbrack
  | Tlbrace
  | Trbrace
  | Tlt
  | Tgt
  | Tcomma
  | Tcolon
  | Tbang
  | Tassign
  | Tstar
  | Tminus
  | Tx
  | Tzeroinitializer
  | Tundef
  | Tattributes
  | Tmetadata
  -- Finalizer
  | Tret
  | Tbr
  | Tswitch
  | Tindirectbr
  | Tinvoke
  | Tresume
  | Tunreachable
  -- Operations
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
  | Tor
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
  | Tfptosi
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

  | Teq
  | Tne
  | Tugt
  | Tuge
  | Tult
  | Tule
  | Tsgt
  | Tsge
  | Tslt
  | Tsle
  | Tfalse
  | Toeq
  | Togt
  | Toge
  | Tolt
  | Tole
  | Tone
  | Tord
  | Tuno
  | Tueq
  | Tune
  | Ttrue

  | Tlabel
  | Tvolatile
  | Tinbounds
  | Talign
  | Tnnan
  | Tninf
  | Tnsz
  | Tarcp
  | Tfast
  | Tto
  | Tnsw
  | Tnuw

  | Ttarget
  | Tdatalayout
  | Ttriple
  | Tdefine
  -- Types
  | Tfloat
  | Tdouble
  | TintegerType Word32
  | Tvoid
  -- Parameter Attributes
  | Tzeroext
  | Tsignext
  | Tinreg
  | Tbyval
  | Tinalloca
  | Tsret
  | Tnoalias
  | Tnocapture
  | Tnest
  | Treturned
  -- Function Attributes
  | Treadonly
  | Tnounwind
  | Tuwtable
  deriving (Eq, Ord, Show)

instance Pretty Token where
    ppr = text . show
  
keywords :: [(String,             Token,            Maybe [Extensions])]
keywords = [("define",            Tdefine,          Nothing),
            ("ret",               Tret,             Nothing),
            ("target",            Ttarget,          Nothing),
            ("datalayout",        Tdatalayout,      Nothing),
            ("triple",            Ttriple,          Nothing),
            ("float",             Tfloat,           Nothing),
            ("icmp",              Ticmp,            Nothing),
            ("add",               Tadd,             Nothing),
            ("fadd",              Tfadd,            Nothing),
            ("sub",               Tsub,             Nothing),
            ("fsub",              Tfsub,            Nothing),
            ("mul",               Tmul,             Nothing),
            ("fmul",              Tfmul,            Nothing),
            ("udiv",              Tudiv,            Nothing),
            ("sdiv",              Tsdiv,            Nothing),
            ("fdiv",              Tfdiv,            Nothing),
            ("urem",              Turem,            Nothing),
            ("srem",              Tsrem,            Nothing),
            ("frem",              Tfrem,            Nothing),
            ("shl",               Tshl,             Nothing),
            ("lshr",              Tlshr,            Nothing),
            ("ashr",              Tashr,            Nothing),
            ("and",               Tand,             Nothing),
            ("or",                Tor,              Nothing),
            ("xor",               Txor,             Nothing),
            ("alloca",            Talloca,          Nothing),
            ("load",              Tload,            Nothing),
            ("store",             Tstore,           Nothing),
            ("getelementptr",     Tgetelementptr,   Nothing),
            ("fence",             Tfence,           Nothing),
            ("cmpxchg",           Tcmpxchg,         Nothing),
            ("atomicrmw",         Tatomicrmw,       Nothing),
            ("trunc",             Ttrunc,           Nothing),
            ("zext",              Tzext,            Nothing),
            ("sext",              Tsext,            Nothing),
            ("fptoui",            Tfptoui,          Nothing),
            ("fptosi",            Tfptosi,          Nothing),
            ("uitofp",            Tuitofp,          Nothing),
            ("sitofp",            Tsitofp,          Nothing),
            ("fptrunc",           Tfptrunc,         Nothing),
            ("fpext",             Tfpext,           Nothing),
            ("ptrtoint",          Tptrtoint,        Nothing),
            ("inttoptr",          Tinttoptr,        Nothing),
            ("bitcast",           Tbitcast,         Nothing),
            ("addrspacecast",     Taddrspacecast,   Nothing),
            ("icmp",              Ticmp,            Nothing),
            ("fcmp",              Tfcmp,            Nothing),
            ("phi",               Tphi,             Nothing),
            ("call",              Tcall,            Nothing),
            ("select",            Tselect,          Nothing),
            ("va_arg",            Tvaarg,           Nothing),
            ("extractelement",    Textractelement,  Nothing),
            ("insertelement",     Tinsertelement,   Nothing),
            ("shufflevector",     Tshufflevector,   Nothing),
            ("extractvalue",      Textractvalue,    Nothing),
            ("insertvalue",       Tinsertvalue,     Nothing),
            ("landingpad",        Tlandingpad,      Nothing),
            ("ret",               Tret,             Nothing),
            ("br",                Tbr,              Nothing),
            ("switch",            Tswitch,          Nothing),
            ("indirectbr",        Tindirectbr,      Nothing),
            ("invoke",            Tinvoke,          Nothing),
            ("resume",            Tresume,          Nothing),
            ("unreachable",       Tunreachable,     Nothing),
            ("label",             Tlabel,           Nothing),
            ("volatile",          Tvolatile,        Nothing),
            ("inbounds",          Tinbounds,        Nothing),
            ("align",             Talign,           Nothing),
            ("nnan",              Tnnan,            Nothing),
            ("ninf",              Tninf,            Nothing),
            ("nsz",               Tnsz,             Nothing),
            ("arcp",              Tarcp,            Nothing),
            ("fast",              Tfast,            Nothing),
            ("eq",                Teq,              Nothing),
            ("ne",                Tne,              Nothing),
            ("ugt",               Tugt,             Nothing),
            ("uge",               Tuge,             Nothing),
            ("ult",               Tult,             Nothing),
            ("ule",               Tule,             Nothing),
            ("sgt",               Tsgt,             Nothing),
            ("sge",               Tsge,             Nothing),
            ("slt",               Tslt,             Nothing),
            ("sle",               Tsle,             Nothing),
            ("false",             Tfalse,           Nothing),
            ("oeq",               Toeq,             Nothing),
            ("ogt",               Togt,             Nothing),
            ("oge",               Toge,             Nothing),
            ("olt",               Tolt,             Nothing),
            ("ole",               Tole,             Nothing),
            ("one",               Tone,             Nothing),
            ("ord",               Tord,             Nothing),
            ("uno",               Tuno,             Nothing),
            ("ueq",               Tueq,             Nothing),
            ("une",               Tune,             Nothing),
            ("true",              Ttrue,            Nothing),
            ("to",                Tto,              Nothing),
            ("nsw",               Tnsw,             Nothing),
            ("nuw",               Tnuw,             Nothing),
            ("zeroext",           Tzeroext,         Nothing),
            ("signext",           Tsignext,         Nothing),
            ("inreg",             Tinreg,           Nothing),
            ("byval",             Tbyval,           Nothing),
            ("inalloca",          Tinalloca,        Nothing),
            ("sret",              Tsret,            Nothing),
            ("noalias",           Tnoalias,         Nothing),
            ("nocapture",         Tnocapture,       Nothing),
            ("nest",              Tnest,            Nothing),
            ("returned",          Treturned,        Nothing),
            ("readonly",          Treadonly,        Nothing),
            ("x",                 Tx,               Nothing),
            ("zeroinitializer",   Tzeroinitializer, Nothing),
            ("undef",             Tundef,           Nothing),
            ("attributes",        Tattributes,      Nothing),
            ("nounwind",          Tnounwind,        Nothing),
            ("uwtable",           Tuwtable,         Nothing),
            ("metadata",          Tmetadata,        Nothing),
            ("double",            Tdouble,          Nothing),
            ("float",             Tfloat,           Nothing),
            ("void",              Tvoid,            Nothing)
           ]

keywordMap :: Map.Map String (Token, Maybe ExtensionsInt)
keywordMap = Map.fromList (map f keywords)
  where
    f  ::  (String, Token, Maybe [Extensions])
       ->  (String, (Token, Maybe ExtensionsInt))
    f (s, t, Nothing)    = (s, (t, Nothing))
    f (s, t, Just exts)  = (s, (t, Just i))
      where
        i = foldl' setBit 0 (map fromEnum exts)