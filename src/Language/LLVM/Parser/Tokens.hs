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
  | Meta
  deriving (Eq, Ord, Show)

data Token
  = Teof
  | TintConst Integer
  | TfloatConst Rational
  | TstringConst String
  | Tnamed Visibility String
  | Tunnamed Visibility Word
  | TjumpLabel String
  | Tlparen
  | Trparen
  | Tlbrack
  | Trbrack
  | Tlbrace
  | Trbrace
  | Tlt
  | Tgt
  | Tcomma
  | Tassign
  | Tstar
  | Tminus
  | Tbang
  | Tpoints
  | Tx
  | Tzeroinitializer
  | Tundef
  | Tglobal
  | Tconstant
  | Talias
  | Tunwind
  | Tunordered
  | Tmonotonic
  | Tacquire
  | Trelease
  | Tacq_rel
  | Tseq_cst
  | Tsinglethread
  | Txchg
  | Tnand
  | Tmax
  | Tmin
  | Tumax
  | Tumin
  | Tcleanup
  | Tcatch
  | Tfilter
  | Tpersonality
  | Tprivate
  | Tinternal
  | Tavailable_externally
  | Tlinkonce
  | Tweak
  | Tcommon
  | Tappending
  | Textern_weak
  | Tlinkonce_odr
  | Tweak_odr
  | Texternal
  | Tdefault
  | Thidden
  | Tprotected
  | Tccc
  | Tfastcc
  | Tcoldcc
  | Tcc
  | Tatomic
  | Tnull
  | Texact
  | Taddrspace
  | Tblockaddress
  | Tmodule
  | Tasm
  | Ttype
  | Topaque
  | Tsideeffect
  | Tinteldialect
  | Tsection
  | Tgc
  | Ttail
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
  | Thalf
  | Tfloat
  | Tdouble
  | TintegerType Word32
  | Tvoid
  | Tmetadata
  -- Parameter Attributes
  | Tzeroext
  | Tsignext
  | Tinreg
  | Tbyval
  | Tsret
  | Tnoalias
  | Tnocapture
  | Tnest
  -- Function Attributes
  | Talignstack
  | Talwaysinline
  | Tinlinehint
  | Tnaked
  | Tnoimplicitfloat
  | Tnoinline
  | Tnonlazybind
  | Tnoredzone
  | Tnoreturn
  | Tnounwind
  | Toptsize
  | Treadnone
  | Treadonly
  | Tssp
  | Tsspreq
  | Tuwtable
  -- Loops
  | Tfor
  | Tin
  | Twith
  | Tas
  -- Anti-Quotation
  | Tanti_dl String
  | Tanti_tt String
  | Tanti_def String
  | Tanti_defs String
  | Tanti_bb String
  | Tanti_bbs String
  | Tanti_instr String
  | Tanti_const String
  | Tanti_id String
  | Tanti_gid String
  | Tanti_param String
  | Tanti_params String
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
            ("sret",              Tsret,            Nothing),
            ("noalias",           Tnoalias,         Nothing),
            ("nest",              Tnest,            Nothing),
            ("x",                 Tx,               Nothing),
            ("zeroinitializer",   Tzeroinitializer, Nothing),
            ("undef",             Tundef,           Nothing),
            ("nounwind",          Tnounwind,        Nothing),
            ("nocapture",         Tnocapture,       Nothing),
            ("double",            Tdouble,          Nothing),
            ("float",             Tfloat,           Nothing),
            ("half",              Thalf,            Nothing),
            ("void",              Tvoid,            Nothing),
            ("metadata",          Tmetadata,        Nothing),
            ("alignstack",        Talignstack,      Nothing),
            ("alwaysinline",      Talwaysinline,    Nothing),
            ("inlinehint",        Tinlinehint,      Nothing),
            ("naked",             Tnaked,           Nothing),
            ("noimplicitfloat",   Tnoimplicitfloat, Nothing),
            ("noinline",          Tnoinline,        Nothing),
            ("nonlazybind",       Tnonlazybind,     Nothing),
            ("noredzone",         Tnoredzone,       Nothing),
            ("noreturn",          Tnoreturn,        Nothing),
            ("nounwind",          Tnounwind,        Nothing),
            ("optsize",           Toptsize,         Nothing),
            ("readnone",          Treadnone,        Nothing),
            ("readonly",          Treadonly,        Nothing),
            ("ssp",               Tssp,             Nothing),
            ("sspreq",            Tsspreq,          Nothing),
            ("uwtable",           Tuwtable,         Nothing),
            ("global",            Tglobal,          Nothing),
            ("constant",          Tconstant,        Nothing),
            ("alias",             Talias,           Nothing),
            ("unwind",            Tunwind,          Nothing),
            ("unordered",         Tunordered,       Nothing),
            ("monotonic",         Tmonotonic,       Nothing),
            ("acquire",           Tacquire,         Nothing),
            ("release",           Trelease,         Nothing),
            ("acq_rel",           Tacq_rel,         Nothing),
            ("seq_cst",           Tseq_cst,         Nothing),
            ("singlethread",      Tsinglethread,    Nothing),
            ("xchg",              Txchg,            Nothing),
            ("nand",              Tnand,            Nothing),
            ("max",               Tmax,             Nothing),
            ("min",               Tmin,             Nothing),
            ("umax",              Tumax,            Nothing),
            ("umin",              Tumin,            Nothing),
            ("cleanup",           Tcleanup,         Nothing),
            ("catch",             Tcatch,           Nothing),
            ("filter",            Tfilter,          Nothing),
            ("personality",       Tpersonality,     Nothing),
            ("private",           Tprivate,         Nothing),
            ("internal",          Tinternal,        Nothing),
            ("available_externally",
                                  Tavailable_externally,
                                                    Nothing),
            ("linkonce",          Tlinkonce,        Nothing),
            ("weak",              Tweak,            Nothing),
            ("common",            Tcommon,          Nothing),
            ("appending",         Tappending,       Nothing),
            ("extern_weak",       Textern_weak,     Nothing),
            ("linkonce_odr",      Tlinkonce_odr,    Nothing),
            ("weak_odr",          Tweak_odr,        Nothing),
            ("external",          Texternal,        Nothing),
            ("default",           Tdefault,         Nothing),
            ("hidden",            Thidden,          Nothing),
            ("protected",         Tprotected,       Nothing),
            ("ccc",               Tccc,             Nothing),
            ("fastcc",            Tfastcc,          Nothing),
            ("coldcc",            Tcoldcc,          Nothing),
            ("cc",                Tcc,              Nothing),
            ("atomic",            Tatomic,          Nothing),
            ("null",              Tnull,            Nothing),
            ("exact",             Texact,           Nothing),
            ("addrspace",         Taddrspace,       Nothing),
            ("blockaddress",      Tblockaddress,    Nothing),
            ("module",            Tmodule,          Nothing),
            ("asm",               Tasm,             Nothing),
            ("type",              Ttype,            Nothing),
            ("opaque",            Topaque,          Nothing),
            ("sideeffect",        Tsideeffect,      Nothing),
            ("inteldialect",      Tinteldialect,    Nothing),
            ("section",           Tsection,         Nothing),
            ("gc",                Tgc,              Nothing),
            ("tail",              Ttail,            Nothing),
            ("for",               Tfor,             Just [Loops]),
            ("in",                Tin,              Just [Loops]),
            ("with",              Twith,            Just [Loops]),
            ("as",                Tas,              Just [Loops])
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