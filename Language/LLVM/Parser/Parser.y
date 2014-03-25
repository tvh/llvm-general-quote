{
module Language.LLVM.Parser.Parser where

import Control.Monad (forM_,
                      when,
                      unless,
                      liftM)
import Control.Monad.Exception
import Data.List (intersperse)
import Data.List.Split
import Data.Loc
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Word
import Text.PrettyPrint.Mainland

import Language.LLVM.Parser.Lexer
import Language.LLVM.Parser.Monad
import qualified Language.LLVM.Parser.Tokens as T
import qualified Language.LLVM.AST as A
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.Linkage as A
import qualified LLVM.General.AST.Visibility as A
import qualified LLVM.General.AST.CallingConvention as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.IntegerPredicate as AI
import qualified LLVM.General.AST.FloatingPointPredicate as AF
}

%token
 INT                 { L _ (T.TintConst $$) }
 FLOAT               { L _ (T.TfloatConst $$) }
 STRING              { L _ (T.TstringConst $$) }
 NAMED_GLOBAL        { L _ (T.Tnamed T.Global $$) }
 NAMED_LOCAL         { L _ (T.Tnamed T.Local $$) }
 UNNAMED_GLOBAL      { L _ (T.Tunnamed T.Global $$) }
 UNNAMED_LOCAL       { L _ (T.Tunnamed T.Local $$) }
 JUMPLABEL           { L _ (T.TjumpLabel $$) }
 '('    { L _ T.Tlparen }
 ')'    { L _ T.Trparen }
 '['    { L _ T.Tlbrack }
 ']'    { L _ T.Trbrack }
 '{'    { L _ T.Tlbrace }
 '}'    { L _ T.Trbrace }
 '<'    { L _ T.Tlt }
 '>'    { L _ T.Tgt }
 ','    { L _ T.Tcomma }
 '='    { L _ T.Tassign }
 '*'    { L _ T.Tstar }
 '-'    { L _ T.Tminus }
 'x'    { L _ T.Tx }
 'zeroinitializer'  { L _ T.Tzeroinitializer }
 'undef'            { L _ T.Tundef }
 'ret'              { L _ T.Tret }
 'br'               { L _ T.Tbr }
 'switch'           { L _ T.Tswitch }
 'indirectbr'       { L _ T.Tindirectbr }
 'invoke'           { L _ T.Tinvoke }
 'resume'           { L _ T.Tresume }
 'unreachable'      { L _ T.Tunreachable }
 'add'              { L _ T.Tadd }
 'fadd'             { L _ T.Tfadd }
 'sub'              { L _ T.Tsub }
 'fsub'             { L _ T.Tfsub }
 'mul'              { L _ T.Tmul }
 'fmul'             { L _ T.Tfmul }
 'udiv'             { L _ T.Tudiv }
 'sdiv'             { L _ T.Tsdiv }
 'fdiv'             { L _ T.Tfdiv }
 'urem'             { L _ T.Turem }
 'srem'             { L _ T.Tsrem }
 'frem'             { L _ T.Tfrem }
 'shl'              { L _ T.Tshl }
 'lshr'             { L _ T.Tlshr }
 'ashr'             { L _ T.Tashr }
 'and'              { L _ T.Tand }
 'or'               { L _ T.Tor }
 'xor'              { L _ T.Txor }
 'alloca'           { L _ T.Talloca }
 'load'             { L _ T.Tload }
 'store'            { L _ T.Tstore }
 'getelementptr'    { L _ T.Tgetelementptr }
 'fence'            { L _ T.Tfence }
 'cmpxchg'          { L _ T.Tcmpxchg }
 'atomicrmw'        { L _ T.Tatomicrmw }
 'trunc'            { L _ T.Ttrunc }
 'zext'             { L _ T.Tzext }
 'sext'             { L _ T.Tsext }
 'fptoui'           { L _ T.Tfptoui }
 'fptosi'           { L _ T.Tfptosi }
 'uitofp'           { L _ T.Tuitofp }
 'sitofp'           { L _ T.Tsitofp }
 'fptrunc'          { L _ T.Tfptrunc }
 'fpext'            { L _ T.Tfpext }
 'ptrtoint'         { L _ T.Tptrtoint }
 'inttoptr'         { L _ T.Tinttoptr }
 'bitcast'          { L _ T.Tbitcast }
 'addrspacecast'    { L _ T.Taddrspacecast }
 'icmp'             { L _ T.Ticmp }
 'fcmp'             { L _ T.Tfcmp }
 'phi'              { L _ T.Tphi }
 'call'             { L _ T.Tcall }
 'select'           { L _ T.Tselect }
 'va_arg'           { L _ T.Tvaarg }
 'extractelement'   { L _ T.Textractelement }
 'insertelement'    { L _ T.Tinsertelement }
 'shufflevector'    { L _ T.Tshufflevector }
 'extractvalue'     { L _ T.Textractvalue }
 'insertvalue'      { L _ T.Tinsertvalue }
 'landingpad'       { L _ T.Tlandingpad }
 'eq'               { L _ T.Teq }
 'ne'               { L _ T.Tne }
 'ugt'              { L _ T.Tugt }
 'uge'              { L _ T.Tuge }
 'ult'              { L _ T.Tult }
 'ule'              { L _ T.Tule }
 'sgt'              { L _ T.Tsgt }
 'sge'              { L _ T.Tsge }
 'slt'              { L _ T.Tslt }
 'sle'              { L _ T.Tsle }
 'false'            { L _ T.Tfalse }
 'oeq'              { L _ T.Toeq }
 'ogt'              { L _ T.Togt }
 'oge'              { L _ T.Toge }
 'olt'              { L _ T.Tolt }
 'ole'              { L _ T.Tole }
 'one'              { L _ T.Tone }
 'ord'              { L _ T.Tord }
 'uno'              { L _ T.Tuno }
 'ueq'              { L _ T.Tueq }
 'une'              { L _ T.Tune }
 'true'             { L _ T.Ttrue }
 'label'            { L _ T.Tlabel }
 'volatile'         { L _ T.Tvolatile }
 'inbounds'         { L _ T.Tinbounds }
 'align'            { L _ T.Talign }
 'nnan'             { L _ T.Tnnan }
 'ninf'             { L _ T.Tninf }
 'nsz'              { L _ T.Tnsz }
 'arcp'             { L _ T.Tarcp }
 'fast'             { L _ T.Tfast }
 'to'               { L _ T.Tto }
 'nsw'              { L _ T.Tnsw }
 'nuw'              { L _ T.Tnuw }
 'target'           { L _ T.Ttarget }
 'datalayout'       { L _ T.Tdatalayout }
 'triple'           { L _ T.Ttriple }
 'define'           { L _ T.Tdefine }
 'void'             { L _ T.Tvoid }
 'float'            { L _ T.Tfloat }
 'double'           { L _ T.Tdouble }
 INTEGERTYPE        { L _ (T.TintegerType $$) }
 'zeroext'          { L _ T.Tzeroext }
 'signext'          { L _ T.Tsignext }
 'inreg'            { L _ T.Tinreg }
 'byval'            { L _ T.Tbyval }
 'sret'             { L _ T.Tsret }
 'noalias'          { L _ T.Tnoalias }
 'nocapture'        { L _ T.Tnocapture }
 'nest'             { L _ T.Tnest }
 'alignstack'       { L _ T.Talignstack }
 'alwaysinline'     { L _ T.Talwaysinline }
 'inlinehint'       { L _ T.Tinlinehint }
 'naked'            { L _ T.Tnaked }
 'noimplicitfloat'  { L _ T.Tnoimplicitfloat }
 'noinline'         { L _ T.Tnoinline }
 'nonlazybind'      { L _ T.Tnonlazybind }
 'noredzone'        { L _ T.Tnoredzone }
 'noreturn'         { L _ T.Tnoreturn }
 'nounwind'         { L _ T.Tnounwind }
 'optsize'          { L _ T.Toptsize }
 'readnone'         { L _ T.Treadnone }
 'readonly'         { L _ T.Treadonly }
 'ssp'              { L _ T.Tssp }
 'sspreq'           { L _ T.Tsspreq }
 'uwtable'          { L _ T.Tuwtable }
 'global'           { L _ T.Tglobal }
 'constant'         { L _ T.Tconstant }
 'alias'            { L _ T.Talias }
 
 ANTI_DEF           { L _ (T.Tanti_def $$) }
 ANTI_DEFS          { L _ (T.Tanti_defs $$) }
 ANTI_BB            { L _ (T.Tanti_bb $$) }
 ANTI_BBS           { L _ (T.Tanti_bbs $$) }
 ANTI_INSTR         { L _ (T.Tanti_instr $$) }
 ANTI_CONST         { L _ (T.Tanti_const $$) }
 ANTI_ID            { L _ (T.Tanti_id $$) }
 ANTI_GID           { L _ (T.Tanti_gid $$) }
 ANTI_PARAM         { L _ (T.Tanti_param $$) }
 ANTI_PARAMS        { L _ (T.Tanti_params $$) }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseModule       module
%name parseDefinition   definition
%name parseBasicBlock   basicBlock
%name parseInstruction  instruction

%%

{------------------------------------------------------------------------------
 -
 - Constants
 -
 -----------------------------------------------------------------------------}

constant :: { A.Type -> A.Constant }
constant :
    INT                   { intConstant $1 }
  | '-' INT               { intConstant (-$2) }
  | FLOAT                 { floatConstant $1 }
  | '-' FLOAT             { floatConstant (-$2) }
  | 'zeroinitializer'     { A.Null }
  | '{' constantList '}'  { \_ -> A.Struct Nothing False (rev $2) }
  | '[' constantList ']'  { \t -> A.Array t (rev $2) }
  | '<' constantList '>'  { \_ -> A.Vector (rev $2) }
  | 'undef'               { A.Undef }
  | globalName            { \_ -> A.GlobalReference $1 }
  | ANTI_CONST            { \_ -> A.AntiConstant $1 }

tConstant :: { A.Constant }
tConstant :
    type constant         { $2 $1 }

constantList :: { RevList A.Constant }
constantList :
    tConstant                    { RCons $1 RNil }
  | constantList ',' tConstant   { RCons $3 $1 }

{------------------------------------------------------------------------------
 -
 - Operands
 -
 -----------------------------------------------------------------------------}

operand :: { A.Type -> A.Operand }
operand :
    constant            { A.ConstantOperand . $1 }
  | name                { \_ -> A.LocalReference $1 }

mOperand :: { A.Type -> Maybe A.Operand }
mOperand :
    {- empty -}      { \_ -> Nothing }
  | operand          { Just . $1 }

tOperand :: { A.Operand }
tOperand :
    type operand        { $2 $1 }

{------------------------------------------------------------------------------
 -
 - Instructions
 -
 -----------------------------------------------------------------------------}

nuw :: { Bool }
nuw :
    {- empty  -}        { False }
  | 'nuw'          { True }

nsw :: { Bool }
nsw :
    {- empty  -}        { False }
  | 'nsw'          { True }

volatile :: { Bool }
volatile :
    {- empty  -}        { False }
  | 'volatile'          { True }

alignment :: { Word32 }
alignment :
    {- empty -}      { 0 }
  | ',' 'align' INT  { fromIntegral $3 }

inBounds :: { Bool }
inBounds :
    {- empty -}         { False }
  | 'inbounds'          { True }

indices :: { RevList A.Operand }
indices :
    {- empty -}            { RNil }
  | indices ',' tOperand   { RCons $3 $1 }

intP :: { AI.IntegerPredicate }
intP :
   'eq'              { AI.EQ }
 | 'ne'              { AI.NE }
 | 'ugt'             { AI.UGT }
 | 'uge'             { AI.UGE }
 | 'ult'             { AI.ULT }
 | 'ule'             { AI.ULE }
 | 'sgt'             { AI.SGT }
 | 'sge'             { AI.SGE }
 | 'slt'             { AI.SLT }
 | 'sle'             { AI.SLE }

fpP :: { AF.FloatingPointPredicate }
fpP :
      'false'          { AF.False }  
    | 'oeq'            { AF.OEQ }  
    | 'ogt'            { AF.OGT }  
    | 'oge'            { AF.OGE }  
    | 'olt'            { AF.OLT }  
    | 'ole'            { AF.OLE }  
    | 'one'            { AF.ONE }  
    | 'ord'            { AF.ORD }  
    | 'uno'            { AF.UNO }  
    | 'ueq'            { AF.UEQ }  
    | 'ugt'            { AF.UGT }  
    | 'uge'            { AF.UGE }  
    | 'ult'            { AF.ULT }  
    | 'ule'            { AF.ULE }  
    | 'une'            { AF.UNE }  
    | 'true'           { AF.True }

phiItem :: { A.Type -> (A.Operand, A.Name) }
phiItem :
    '[' operand ',' name ']'     { \t -> ($2 t, $4) }

phiList :: { A.Type -> RevList (A.Operand, A.Name) }
phiList :
    phiItem                { \t -> RCons ($1 t) RNil }
  | phiList ',' phiItem    { \t -> RCons ($3 t) ($1 t) }

argument :: { (A.Operand, [A.ParameterAttribute]) }
argument :
    tOperand      { ($1, []) }

argumentList :: { RevList (A.Operand, [A.ParameterAttribute]) }
argumentList :
    {- empty -}                      { RNil }
  | argument                         { RCons $1 RNil }
  | argumentList ',' argument        { RCons $3 $1 }

idx :: { Word32 }
idx :
    INT               { fromIntegral $1 }

idxs :: { RevList Word32 }
idxs :
    idx            { RCons $1 RNil }
  | idxs ',' idx   { RCons $3 $1 }

instruction :: { A.Instruction }
instruction :
    'add' nuw nsw type operand ',' operand  { A.Add $2 $3 ($5 $4) ($7 $4) [] }
  | 'fadd' type operand ',' operand         { A.FAdd ($3 $2) ($5 $2) [] }
  | 'sub' nuw nsw type operand ',' operand  { A.Sub $2 $3 ($5 $4) ($7 $4) [] }
  | 'fsub' type operand ',' operand         { A.FSub ($3 $2) ($5 $2) [] }
  | 'mul' nuw nsw type operand ',' operand  { A.Mul $2 $3 ($5 $4) ($7 $4) [] }
  | 'fmul' type operand ',' operand         { A.FMul ($3 $2) ($5 $2) [] }
  | 'udiv' type operand ',' operand         { A.UDiv False ($3 $2) ($5 $2) [] }
  | 'sdiv' type operand ',' operand         { A.SDiv False ($3 $2) ($5 $2) [] }
  | 'fdiv' type operand ',' operand         { A.FDiv ($3 $2) ($5 $2) [] }
  | 'urem' type operand ',' operand         { A.URem ($3 $2) ($5 $2) [] }
  | 'srem' type operand ',' operand         { A.SRem ($3 $2) ($5 $2) [] }
  | 'frem' type operand ',' operand         { A.FRem ($3 $2) ($5 $2) [] }
  | 'shl' nuw nsw type operand ',' operand  { A.Shl $2 $3 ($5 $4) ($7 $4) [] }
  | 'lshr' type operand ',' operand         { A.LShr False ($3 $2) ($5 $2) [] }
  | 'ashr' type operand ',' operand         { A.AShr False ($3 $2) ($5 $2) [] }
  | 'and' type operand ',' operand          { A.And ($3 $2) ($5 $2) [] }
  | 'or' type operand ',' operand           { A.Or ($3 $2) ($5 $2) [] }
  | 'xor' type operand ',' operand          { A.Xor ($3 $2) ($5 $2) [] }
  | 'alloca' type mOperand alignment        { A.Alloca $2 ($3 $2) $4 [] }
  | 'load' volatile tOperand alignment      { A.Load $2 $3 Nothing $4 [] }
  | 'store' volatile tOperand ',' tOperand alignment 
                                            { A.Store $2 $5 $3 Nothing $6 [] }
  | 'getelementptr' inBounds tOperand indices
                                            { A.GetElementPtr $2 $3 (rev $4) [] }
  | 'trunc' tOperand 'to' type              { A.Trunc $2 $4 [] }
  | 'zext' tOperand 'to' type               { A.ZExt $2 $4 [] }
  | 'sext' tOperand 'to' type               { A.SExt $2 $4 [] }
  | 'fptoui' tOperand 'to' type             { A.FPToUI $2 $4 [] }
  | 'fptosi' tOperand 'to' type             { A.FPToSI $2 $4 [] }
  | 'uitofp' tOperand 'to' type             { A.UIToFP $2 $4 [] }
  | 'sitofp' tOperand 'to' type             { A.SIToFP $2 $4 [] }
  | 'fptrunc' tOperand 'to' type            { A.FPTrunc $2 $4 [] }
  | 'fpext' tOperand 'to' type              { A.FPExt $2 $4 [] }
  | 'ptrtoint' tOperand 'to' type           { A.PtrToInt $2 $4 [] }
  | 'inttoptr' tOperand 'to' type           { A.IntToPtr $2 $4 [] }
  | 'bitcast' tOperand 'to' type            { A.BitCast $2 $4 [] }
  | 'addrspacecast' tOperand 'to' type      { A.AddrSpaceCast $2 $4 [] }
  | 'icmp' intP type operand ',' operand    { A.ICmp $2 ($4 $3) ($6 $3) [] }
  | 'fcmp' fpP type operand ',' operand     { A.FCmp $2 ($4 $3) ($6 $3) [] }
  | 'phi' type phiList                      { A.Phi $2 (rev ($3 $2)) [] }
  | 'call' pAttributes type operand '(' argumentList ')'
                                            { A.Call False A.C (rev $2) (Right ($4 $3)) (rev $6) [] [] }
  | 'select' tOperand ',' tOperand ',' tOperand
                                            { A.Select $2 $4 $6 [] }
  | 'va_arg' tOperand ',' type              { A.VAArg $2 $4 [] }
  | 'extractelement' tOperand ',' tOperand  { A.ExtractElement $2 $4 [] }
  | 'insertelement' tOperand ',' tOperand ',' tOperand
                                            { A.InsertElement $2 $4 $6 [] }
  | 'shufflevector' tOperand ',' tOperand ',' type constant
                                            { A.ShuffleVector $2 $4 ($7 $6) [] }
  | 'extractvalue' tOperand ',' idxs        { A.ExtractValue $2 (rev $4) [] }
  | 'insertvalue' tOperand ',' tOperand ',' idxs
                                            { A.InsertValue $2 $4 (rev $6) [] }
  | ANTI_INSTR                              { A.AntiInstruction $1 }

name :: { A.Name }
name :
    NAMED_LOCAL     { A.Name $1 }
  | UNNAMED_LOCAL   { A.UnName $1 }
  | ANTI_ID         { A.AntiName $1 }

namedI :: { A.Named A.Instruction }
namedI :
    instruction                     { A.Do $1 }
  | name '=' instruction            { $1 A.:= $3 }

instructions :: { RevList (A.Named A.Instruction) } 
instructions :
    {- empty -}                   { RNil }
  | instructions namedI           { RCons $2 $1 }

destination :: { (A.Constant, A.Name) }
destination :
    tConstant ',' label    { ($1, $3) }

destinations :: { RevList (A.Constant, A.Name) }
destinations :
    {- empty -}                   { RNil }
  | destinations destination      { RCons $2 $1 }

label :: { A.Name }
label :
  'label' name        { $2 }

labels :: { RevList A.Name }
labels :
    label                         { RCons $1 RNil }
  | labels ','label               { RCons $3 $1 }

namedT :: { A.Named A.Terminator }
namedT :
    terminator                      { A.Do $1 }
  | name   '=' terminator           { $1 A.:= $3 }

terminator :: { A.Terminator }
terminator :
    'ret'                 { A.Ret Nothing [] }
  | 'ret' type operand    { A.Ret (Just ($3 $2)) [] }
  | 'br' 'label' name     { A.Br $3 [] }
  | 'br' type operand ',' 'label' name ',' 'label' name
                          { A.CondBr ($3 $2) $6 $9 [] }
  | 'switch' type operand ',' 'label' name '[' destinations ']'
                          { A.Switch ($3 $2) $6 (rev $8) [] }
  | 'indirectbr' tOperand ',' '[' labels ']'
                          { A.IndirectBr $2 (rev $5) [] }
  | 'resume' tOperand     { A.Resume $2 [] }
  | 'unreachable'         { A.Unreachable [] }

{------------------------------------------------------------------------------
 -
 - Basic Blocks
 -
 -----------------------------------------------------------------------------}

basicBlock :: { A.BasicBlock }
basicBlock :
    JUMPLABEL instructions namedT     { A.BasicBlock (A.Name $1) (rev $2) $3 }
  | instructions namedT               { A.BasicBlock (A.UnName 0) (rev $1) $2 }
  | ANTI_BB                           { A.AntiBasicBlock $1 }
  | ANTI_BBS                          { A.AntiBasicBlockList $1 }

basicBlocks :: { RevList (A.BasicBlock) }
basicBlocks :
    {- empty -}             { RNil }
  | basicBlocks basicBlock  { RCons $2 $1 }

{------------------------------------------------------------------------------
 -
 - Global Definitions
 -
 -----------------------------------------------------------------------------}

globalName :: { A.Name }
globalName :
    NAMED_GLOBAL     { A.Name $1 }
  | UNNAMED_GLOBAL   { A.UnName $1 }
  | ANTI_GID         { A.AntiName $1 }

type :: { A.Type }
type :
    'void'                    { A.VoidType }
  | INTEGERTYPE               { A.IntegerType $1 }
  | 'double'                  { A.FloatingPointType 64 A.IEEE }
  | 'float'                   { A.FloatingPointType 32 A.IEEE }
  | type '*'                  { A.PointerType $1 (A.AddrSpace 0) }
  | type '(' typeList ')'     { A.FunctionType $1 (rev $3) False }
  | '<' INT 'x' type '>'      { A.VectorType (fromIntegral $2) $4 }
  | '{' typeList '}'          { A.StructureType False (rev $2) }
  | '<' '{' typeList '}' '>'  { A.StructureType True (rev $3) }
  | '[' INT 'x' type ']'      { A.ArrayType (fromIntegral $2) $4 }

typeList :: { RevList A.Type }
typeList :
    {- empty -}                      { RNil }
  | type                             { RCons $1 RNil }
  | typeList ',' type                { RCons $3 $1 }

pAttribute :: { A.ParameterAttribute }
pAttribute :
    'nocapture'         { A.NoCapture }

pAttributes :: { RevList A.ParameterAttribute }
pAttributes :
    {- empty -}                { RNil }
  | pAttributes pAttribute     { RCons $2 $1 }

parameter :: { A.Parameter }
parameter :
    type pAttributes name         { A.Parameter $1 $3 (rev $2) }
  | ANTI_PARAM                    { A.AntiParameter $1 }
  | ANTI_PARAMS                   { A.AntiParameterList $1 }

parameterList :: { RevList A.Parameter }
parameterList :
    {- empty -}                      { RNil }
  | parameter                        { RCons $1 RNil }
  | parameterList ',' parameter      { RCons $3 $1 }

fAttribute :: { A.FunctionAttribute }    
fAttribute :
    'alignstack' '(' INT ')'         { A.StackAlignment (fromIntegral $3) }
  | 'alwaysinline'                   { A.AlwaysInline }
  | 'inlinehint'                     { A.InlineHint }
  | 'naked'                          { A.Naked }
  | 'noimplicitfloat'                { A.NoImplicitFloat }
  | 'noinline'                       { A.NoInline }
  | 'nonlazybind'                    { A.NonLazyBind }
  | 'noredzone'                      { A.NoRedZone }
  | 'noreturn'                       { A.NoReturn }
  | 'nounwind'                       { A.NoUnwind }
  | 'optsize'                        { A.OptimizeForSize }
  | 'readnone'                       { A.ReadNone }
  | 'readonly'                       { A.ReadOnly }
  | 'ssp'                            { A.StackProtect }
  | 'sspreq'                         { A.StackProtectReq }
  | 'uwtable'                        { A.UWTable }

fAttributes :: { RevList A.FunctionAttribute }
fAttributes :
    {- empty -}                   { RNil }
  | fAttributes fAttribute        { RCons $2 $1 }

isConstant :: { Bool }
isConstant :
    'global'        { False }
  | 'constant'      { True }

global :: { A.Global }
global :
    'define' type globalName '(' parameterList ')' fAttributes '{' basicBlocks '}'
      { A.Function A.External A.Default A.C [] $2 $3 ([], False) [] Nothing 0 Nothing (rev $9) }
  | globalName '=' isConstant type alignment
      { A.GlobalVariable $1 A.External A.Default False (A.AddrSpace 0) False $3 $4 Nothing Nothing $5 }
  | globalName '=' 'alias' type constant
      { A.GlobalAlias $1 A.External A.Default $4 ($5 $4) }

{------------------------------------------------------------------------------
 -
 - Definitions
 -
 -----------------------------------------------------------------------------}

definition :: { A.Definition }
definition :
    global         { A.GlobalDefinition $1 }
  | ANTI_DEF       { A.AntiDefinition $1 }
  | ANTI_DEFS      { A.AntiDefinitionList $1 }

definitions :: { RevList A.Definition }
definitions :
    {- empty -}             { RNil }
  | definitions definition  { RCons $2 $1 }

{------------------------------------------------------------------------------
 -
 - Modules
 -
 -----------------------------------------------------------------------------}

dataLayout :: { Maybe A.DataLayout }
dataLayout :
    {- empty -}                      { Nothing }
  | 'target' 'datalayout' '=' STRING { Just (dataLayout $4) }

targetTriple :: { Maybe String }
targetTriple :
    {- empty -}                       { Nothing }
  | 'target' 'triple' '=' STRING      { Just $4 } 

module :: { A.Module }
module :
    dataLayout targetTriple definitions  { A.Module "" $1 $2 (rev $3) }

{
intConstant :: Integer -> A.Type -> A.Constant
intConstant n (A.IntegerType bs) = A.Int bs n

floatConstant :: Rational -> A.Type -> A.Constant
floatConstant x (A.FloatingPointType 32 _) = A.Float (A.Single (fromRational $2))
floatConstant x (A.FloatingPointType 64 _) = A.Float (A.Double (fromRational $2))

dataLayout :: String -> A.DataLayout
dataLayout s = A.DataLayout endianness stackAlignment pointerLayouts typeLayouts nativeSizes
 where
  infos :: [String]
  infos = splitOn "-" s
  endianness :: Maybe A.Endianness
  endianness = listToMaybe $ do
    [c] <- infos
    case c of
      'E' -> return A.BigEndian
      'e' -> return A.LittleEndian
      _   -> []
  stackAlignment :: Maybe Word32
  stackAlignment = listToMaybe $ do
    ('S':s) <- infos
    (n,"") <- reads s
    return n
  pointerLayouts :: M.Map A.AddrSpace (Word32, A.AlignmentInfo)
  pointerLayouts = M.fromList $ do
    ('p':s@(x:_)) <- infos
    let parts = splitOn ":" s
    (n,size,abi,pref) <- case parts of
      [s,size,abi,pref] -> do
        (n,"") <- reads s
        return (n,size,abi,pref)
      [size,abi,pref] -> return (0,size,abi,pref)
      _ -> []
    (size',"") <- reads size
    (abi',"") <- reads abi
    (pref',"") <- reads pref
    return (A.AddrSpace n, (size', A.AlignmentInfo abi' (Just pref')))
  typeLayouts :: M.Map (A.AlignType, Word32) A.AlignmentInfo
  typeLayouts = M.fromList $ do
    [(t:size),abi,pref] <- map (splitOn ":") infos
    k <- case t of
      'i' -> reads size >>= \(size,"") -> return (A.IntegerAlign, size)
      'v' -> reads size >>= \(size,"") -> return (A.VectorAlign, size)
      'f' -> reads size >>= \(size,"") -> return (A.FloatAlign, size)
      's' -> reads size >>= \(size,"") -> return (A.StackAlign, size)
      'a' -> return (A.AggregateAlign, 0)
      _ -> []
    (abi',"") <- reads abi
    (pref',"") <- reads pref
    return (k, A.AlignmentInfo abi' (Just pref'))
  nativeSizes :: Maybe (S.Set Word32)
  nativeSizes = Just $ S.fromList $ do
    ('n':s) <- infos
    size <- splitOn ":" s
    (size',"") <- reads size
    return size'


happyError :: L T.Token -> P a
happyError (L loc t) =
    parserError (locStart loc) (text "parse error on" <+> quoteTok (ppr t))

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    t <- lexToken
    setCurToken t
    cont t

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data RevList a  =  RNil
                |  RCons a (RevList a)

rnil :: RevList a
rnil = RNil

rsingleton :: a -> RevList a
rsingleton x = RCons x RNil

rcons :: a -> RevList a -> RevList a
rcons x xs  = RCons x xs

rev :: RevList a -> [a]
rev xs = go [] xs
  where
    go  l  RNil          = l
    go  l  (RCons x xs)  = go (x : l) xs
}