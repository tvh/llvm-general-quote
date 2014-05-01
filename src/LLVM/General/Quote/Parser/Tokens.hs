module LLVM.General.Quote.Parser.Tokens (
    Token(..),
    Visibility(..),
    keywords,
    keywordMap
  ) where

import qualified Data.Map as Map
import Data.Word
import Text.PrettyPrint.Mainland

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
  deriving (Eq, Ord)

instance Show Token where
  show (TintConst _) = "INT"
  show (TfloatConst _) = "FLOAT"
  show (TstringConst _) = "STRING"
  show (Tnamed Global _) = "NAMED_GLOBAL"
  show (Tnamed Local _) = "NAMED_LOCAL"
  show (Tnamed Meta _) = "NAMED_META"
  show (Tunnamed Global _) = "UNNAMED_GLOBAL"
  show (Tunnamed Local _) = "UNNAMED_LOCAL"
  show (Tunnamed Meta _) = "UNNAMED_META"
  show (TjumpLabel _) = "JUMPLABEL"
  show (TintegerType _) = "INTEGERTYPE"
  show Tlparen = "("
  show Trparen = ")"
  show Tlbrack = "["
  show Trbrack = "]"
  show Tlbrace = "{"
  show Trbrace = "}"
  show Tlt = "<"
  show Tgt = ">"
  show Tcomma = ","
  show Tassign = "="
  show Tstar = "*"
  show Tminus = "-"
  show Tbang = "!"
  show Tpoints = "..."
  show Tx = "x"
  show Tzeroinitializer = "zeroinitializer"
  show Tundef = "undef"
  show Tret = "ret"
  show Tbr = "br"
  show Tswitch = "switch"
  show Tindirectbr = "indirectbr"
  show Tinvoke = "invoke"
  show Tresume = "resume"
  show Tunreachable = "unreachable"
  show Tadd = "add"
  show Tfadd = "fadd"
  show Tsub = "sub"
  show Tfsub = "fsub"
  show Tmul = "mul"
  show Tfmul = "fmul"
  show Tudiv = "udiv"
  show Tsdiv = "sdiv"
  show Tfdiv = "fdiv"
  show Turem = "urem"
  show Tsrem = "srem"
  show Tfrem = "frem"
  show Tshl = "shl"
  show Tlshr = "lshr"
  show Tashr = "ashr"
  show Tand = "and"
  show Tor = "or"
  show Txor = "xor"
  show Talloca = "alloca"
  show Tload = "load"
  show Tstore = "store"
  show Tgetelementptr = "getelementptr"
  show Tfence = "fence"
  show Tcmpxchg = "cmpxchg"
  show Tatomicrmw = "atomicrmw"
  show Ttrunc = "trunc"
  show Tzext = "zext"
  show Tsext = "sext"
  show Tfptoui = "fptoui"
  show Tfptosi = "fptosi"
  show Tuitofp = "uitofp"
  show Tsitofp = "sitofp"
  show Tfptrunc = "fptrunc"
  show Tfpext = "fpext"
  show Tptrtoint = "ptrtoint"
  show Tinttoptr = "inttoptr"
  show Tbitcast = "bitcast"
  show Taddrspacecast = "addrspacecast"
  show Ticmp = "icmp"
  show Tfcmp = "fcmp"
  show Tphi = "phi"
  show Tcall = "call"
  show Tselect = "select"
  show Tvaarg = "va_arg"
  show Textractelement = "extractelement"
  show Tinsertelement = "insertelement"
  show Tshufflevector = "shufflevector"
  show Textractvalue = "extractvalue"
  show Tinsertvalue = "insertvalue"
  show Tlandingpad = "landingpad"
  show Teq = "eq"
  show Tne = "ne"
  show Tugt = "ugt"
  show Tuge = "uge"
  show Tult = "ult"
  show Tule = "ule"
  show Tsgt = "sgt"
  show Tsge = "sge"
  show Tslt = "slt"
  show Tsle = "sle"
  show Tfalse = "false"
  show Toeq = "oeq"
  show Togt = "ogt"
  show Toge = "oge"
  show Tolt = "olt"
  show Tole = "ole"
  show Tone = "one"
  show Tord = "ord"
  show Tuno = "uno"
  show Tueq = "ueq"
  show Tune = "une"
  show Ttrue = "true"
  show Tlabel = "label"
  show Tvolatile = "volatile"
  show Tinbounds = "inbounds"
  show Talign = "align"
  show Tnnan = "nnan"
  show Tninf = "ninf"
  show Tnsz = "nsz"
  show Tarcp = "arcp"
  show Tfast = "fast"
  show Tto = "to"
  show Tnsw = "nsw"
  show Tnuw = "nuw"
  show Ttarget = "target"
  show Tdatalayout = "datalayout"
  show Ttriple = "triple"
  show Tdefine = "define"
  show Tvoid = "void"
  show Thalf = "half"
  show Tfloat = "float"
  show Tdouble = "double"
  show Tmetadata = "metadata"
  show Tzeroext = "zeroext"
  show Tsignext = "signext"
  show Tinreg = "inreg"
  show Tbyval = "byval"
  show Tsret = "sret"
  show Tnoalias = "noalias"
  show Tnocapture = "nocapture"
  show Tnest = "nest"
  show Talignstack = "alignstack"
  show Talwaysinline = "alwaysinline"
  show Tinlinehint = "inlinehint"
  show Tnaked = "naked"
  show Tnoimplicitfloat = "noimplicitfloat"
  show Tnoinline = "noinline"
  show Tnonlazybind = "nonlazybind"
  show Tnoredzone = "noredzone"
  show Tnoreturn = "noreturn"
  show Tnounwind = "nounwind"
  show Toptsize = "optsize"
  show Treadnone = "readnone"
  show Treadonly = "readonly"
  show Tssp = "ssp"
  show Tsspreq = "sspreq"
  show Tuwtable = "uwtable"
  show Tglobal = "global"
  show Tconstant = "constant"
  show Talias = "alias"
  show Tunwind = "unwind"
  show Tunordered = "unordered"
  show Tmonotonic = "monotonic"
  show Tacquire = "acquire"
  show Trelease = "release"
  show Tacq_rel = "acq_rel"
  show Tseq_cst = "seq_cst"
  show Tsinglethread = "singlethread"
  show Txchg = "xchg"
  show Tnand = "nand"
  show Tmax = "max"
  show Tmin = "min"
  show Tumax = "umax"
  show Tumin = "umin"
  show Tcleanup = "cleanup"
  show Tcatch = "catch"
  show Tfilter = "filter"
  show Tpersonality = "personality"
  show Tprivate = "private"
  show Tinternal = "internal"
  show Tavailable_externally = "available_externall"
  show Tlinkonce = "linkonce"
  show Tweak = "weak"
  show Tcommon = "common"
  show Tappending = "appending"
  show Textern_weak = "extern_weak"
  show Tlinkonce_odr = "linkonce_odr"
  show Tweak_odr = "weak_odr"
  show Texternal = "external"
  show Tdefault = "default"
  show Thidden = "hidden"
  show Tprotected = "protected"
  show Tccc = "ccc"
  show Tfastcc = "fastcc"
  show Tcoldcc = "coldcc"
  show Tcc = "cc"
  show Tatomic = "atomic"
  show Tnull = "null"
  show Texact = "exact"
  show Taddrspace = "addrspace"
  show Tblockaddress = "blockaddress"
  show Tmodule = "module"
  show Tasm = "asm"
  show Ttype = "type"
  show Topaque = "opaque"
  show Tsideeffect = "sideeffect"
  show Tinteldialect = "inteldialect"
  show Tsection = "section"
  show Tgc = "gc"
  show Ttail = "tail"
  show Teof = "EOF"

instance Pretty Token where
    ppr = text . show

keywords :: [(String,             Token)]
keywords = [("define",            Tdefine),
            ("ret",               Tret),
            ("target",            Ttarget),
            ("datalayout",        Tdatalayout),
            ("triple",            Ttriple),
            ("float",             Tfloat),
            ("icmp",              Ticmp),
            ("add",               Tadd),
            ("fadd",              Tfadd),
            ("sub",               Tsub),
            ("fsub",              Tfsub),
            ("mul",               Tmul),
            ("fmul",              Tfmul),
            ("udiv",              Tudiv),
            ("sdiv",              Tsdiv),
            ("fdiv",              Tfdiv),
            ("urem",              Turem),
            ("srem",              Tsrem),
            ("frem",              Tfrem),
            ("shl",               Tshl),
            ("lshr",              Tlshr),
            ("ashr",              Tashr),
            ("and",               Tand),
            ("or",                Tor),
            ("xor",               Txor),
            ("alloca",            Talloca),
            ("load",              Tload),
            ("store",             Tstore),
            ("getelementptr",     Tgetelementptr),
            ("fence",             Tfence),
            ("cmpxchg",           Tcmpxchg),
            ("atomicrmw",         Tatomicrmw),
            ("trunc",             Ttrunc),
            ("zext",              Tzext),
            ("sext",              Tsext),
            ("fptoui",            Tfptoui),
            ("fptosi",            Tfptosi),
            ("uitofp",            Tuitofp),
            ("sitofp",            Tsitofp),
            ("fptrunc",           Tfptrunc),
            ("fpext",             Tfpext),
            ("ptrtoint",          Tptrtoint),
            ("inttoptr",          Tinttoptr),
            ("bitcast",           Tbitcast),
            ("addrspacecast",     Taddrspacecast),
            ("icmp",              Ticmp),
            ("fcmp",              Tfcmp),
            ("phi",               Tphi),
            ("call",              Tcall),
            ("select",            Tselect),
            ("va_arg",            Tvaarg),
            ("extractelement",    Textractelement),
            ("insertelement",     Tinsertelement),
            ("shufflevector",     Tshufflevector),
            ("extractvalue",      Textractvalue),
            ("insertvalue",       Tinsertvalue),
            ("landingpad",        Tlandingpad),
            ("ret",               Tret),
            ("br",                Tbr),
            ("switch",            Tswitch),
            ("indirectbr",        Tindirectbr),
            ("invoke",            Tinvoke),
            ("resume",            Tresume),
            ("unreachable",       Tunreachable),
            ("label",             Tlabel),
            ("volatile",          Tvolatile),
            ("inbounds",          Tinbounds),
            ("align",             Talign),
            ("nnan",              Tnnan),
            ("ninf",              Tninf),
            ("nsz",               Tnsz),
            ("arcp",              Tarcp),
            ("fast",              Tfast),
            ("eq",                Teq),
            ("ne",                Tne),
            ("ugt",               Tugt),
            ("uge",               Tuge),
            ("ult",               Tult),
            ("ule",               Tule),
            ("sgt",               Tsgt),
            ("sge",               Tsge),
            ("slt",               Tslt),
            ("sle",               Tsle),
            ("false",             Tfalse),
            ("oeq",               Toeq),
            ("ogt",               Togt),
            ("oge",               Toge),
            ("olt",               Tolt),
            ("ole",               Tole),
            ("one",               Tone),
            ("ord",               Tord),
            ("uno",               Tuno),
            ("ueq",               Tueq),
            ("une",               Tune),
            ("true",              Ttrue),
            ("to",                Tto),
            ("nsw",               Tnsw),
            ("nuw",               Tnuw),
            ("zeroext",           Tzeroext),
            ("signext",           Tsignext),
            ("inreg",             Tinreg),
            ("byval",             Tbyval),
            ("sret",              Tsret),
            ("noalias",           Tnoalias),
            ("nest",              Tnest),
            ("x",                 Tx),
            ("zeroinitializer",   Tzeroinitializer),
            ("undef",             Tundef),
            ("nounwind",          Tnounwind),
            ("nocapture",         Tnocapture),
            ("double",            Tdouble),
            ("float",             Tfloat),
            ("half",              Thalf),
            ("void",              Tvoid),
            ("metadata",          Tmetadata),
            ("alignstack",        Talignstack),
            ("alwaysinline",      Talwaysinline),
            ("inlinehint",        Tinlinehint),
            ("naked",             Tnaked),
            ("noimplicitfloat",   Tnoimplicitfloat),
            ("noinline",          Tnoinline),
            ("nonlazybind",       Tnonlazybind),
            ("noredzone",         Tnoredzone),
            ("noreturn",          Tnoreturn),
            ("nounwind",          Tnounwind),
            ("optsize",           Toptsize),
            ("readnone",          Treadnone),
            ("readonly",          Treadonly),
            ("ssp",               Tssp),
            ("sspreq",            Tsspreq),
            ("uwtable",           Tuwtable),
            ("global",            Tglobal),
            ("constant",          Tconstant),
            ("alias",             Talias),
            ("unwind",            Tunwind),
            ("unordered",         Tunordered),
            ("monotonic",         Tmonotonic),
            ("acquire",           Tacquire),
            ("release",           Trelease),
            ("acq_rel",           Tacq_rel),
            ("seq_cst",           Tseq_cst),
            ("singlethread",      Tsinglethread),
            ("xchg",              Txchg),
            ("nand",              Tnand),
            ("max",               Tmax),
            ("min",               Tmin),
            ("umax",              Tumax),
            ("umin",              Tumin),
            ("cleanup",           Tcleanup),
            ("catch",             Tcatch),
            ("filter",            Tfilter),
            ("personality",       Tpersonality),
            ("private",           Tprivate),
            ("internal",          Tinternal),
            ("available_externally",
                                  Tavailable_externally),
            ("linkonce",          Tlinkonce),
            ("weak",              Tweak),
            ("common",            Tcommon),
            ("appending",         Tappending),
            ("extern_weak",       Textern_weak),
            ("linkonce_odr",      Tlinkonce_odr),
            ("weak_odr",          Tweak_odr),
            ("external",          Texternal),
            ("default",           Tdefault),
            ("hidden",            Thidden),
            ("protected",         Tprotected),
            ("ccc",               Tccc),
            ("fastcc",            Tfastcc),
            ("coldcc",            Tcoldcc),
            ("cc",                Tcc),
            ("atomic",            Tatomic),
            ("null",              Tnull),
            ("exact",             Texact),
            ("addrspace",         Taddrspace),
            ("blockaddress",      Tblockaddress),
            ("module",            Tmodule),
            ("asm",               Tasm),
            ("type",              Ttype),
            ("opaque",            Topaque),
            ("sideeffect",        Tsideeffect),
            ("inteldialect",      Tinteldialect),
            ("section",           Tsection),
            ("gc",                Tgc),
            ("tail",              Ttail)
           ]

keywordMap :: Map.Map String Token
keywordMap = Map.fromList keywords
