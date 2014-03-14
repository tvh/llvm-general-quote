{
module Language.LLVM.Parser.Parser where

import Control.Monad (forM_,
                      when,
                      unless,
                      liftM)
import Control.Monad.Exception
import Data.List (intersperse)
import Data.Loc
import Data.Maybe (fromMaybe, catMaybes)
import Text.PrettyPrint.Mainland

import Language.LLVM.Parser.Lexer
import Language.LLVM.Parser.Monad
import qualified Language.LLVM.Parser.Tokens as T
import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Constant as A 
  (Constant(Int, Float, Null, Struct, Array, Vector, Undef, BlockAddress, GlobalReference))
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.Instruction as A
import qualified LLVM.General.AST.Operand as A
import qualified LLVM.General.AST.Name as A
import qualified LLVM.General.AST.Global as A
import qualified LLVM.General.AST.Type as A
import qualified LLVM.General.AST.Linkage as A
import qualified LLVM.General.AST.Visibility as A
import qualified LLVM.General.AST.CallingConvention as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.IntegerPredicate as A
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
 AGNUMBER            { L _ (T.TattrGroupNumber $$) }
 MDNAME              { L _ (T.TmetaDataName $$) }
 MDNUMBER            { L _ (T.TmetaDataNumber $$) }
 '('    { L _ T.Tlparen }
 ')'    { L _ T.Trparen }
 '['    { L _ T.Tlbrack }
 ']'    { L _ T.Trbrack }
 '{'    { L _ T.Tlbrace }
 '}'    { L _ T.Trbrace }
 '<'    { L _ T.Tlt }
 '>'    { L _ T.Tgt }
 ','    { L _ T.Tcomma }
 ':'    { L _ T.Tcolon }
 '!'    { L _ T.Tbang }
 '='    { L _ T.Tassign }
 '*'    { L _ T.Tstar }
 '-'    { L _ T.Tminus }
 'x'    { L _ T.Tx }
 'zeroinitializer'             { L _ T.Tzeroinitializer }
 'undef'             { L _ T.Tundef }
 'attributes'             { L _ T.Tattributes }
 'metadata'             { L _ T.Tmetadata }
 'ret'             { L _ T.Tret }
 'br'             { L _ T.Tbr }
 'switch'             { L _ T.Tswitch }
 'indirectbr'             { L _ T.Tindirectbr }
 'invoke'             { L _ T.Tinvoke }
 'resume'             { L _ T.Tresume }
 'unreachable'             { L _ T.Tunreachable }
 'add'             { L _ T.Tadd }
 'fadd'             { L _ T.Tfadd }
 'sub'             { L _ T.Tsub }
 'fsub'             { L _ T.Tfsub }
 'mul'             { L _ T.Tmul }
 'fmul'             { L _ T.Tfmul }
 'udiv'             { L _ T.Tudiv }
 'sdiv'             { L _ T.Tsdiv }
 'fdiv'             { L _ T.Tfdiv }
 'urem'             { L _ T.Turem }
 'srem'             { L _ T.Tsrem }
 'frem'             { L _ T.Tfrem }
 'shl'             { L _ T.Tshl }
 'lshr'             { L _ T.Tlshr }
 'ashr'             { L _ T.Tashr }
 'and'             { L _ T.Tand }
 'or'             { L _ T.Tor }
 'xor'             { L _ T.Txor }
 'alloca'             { L _ T.Talloca }
 'load'             { L _ T.Tload }
 'store'             { L _ T.Tstore }
 'getelementptr'             { L _ T.Tgetelementptr }
 'fence'             { L _ T.Tfence }
 'cmpxchg'             { L _ T.Tcmpxchg }
 'atomicrmw'             { L _ T.Tatomicrmw }
 'trunc'             { L _ T.Ttrunc }
 'zext'             { L _ T.Tzext }
 'sext'             { L _ T.Tsext }
 'fptoui'             { L _ T.Tfptoui }
 'fttosi'             { L _ T.Tfttosi }
 'uitofp'             { L _ T.Tuitofp }
 'sitofp'             { L _ T.Tsitofp }
 'fptrunc'             { L _ T.Tfptrunc }
 'fpext'             { L _ T.Tfpext }
 'ptrtoint'             { L _ T.Tptrtoint }
 'inttoptr'             { L _ T.Tinttoptr }
 'bitcast'             { L _ T.Tbitcast }
 'addrspacecast'             { L _ T.Taddrspacecast }
 'icmp'             { L _ T.Ticmp }
 'fcmp'             { L _ T.Tfcmp }
 'phi'             { L _ T.Tphi }
 'call'             { L _ T.Tcall }
 'select'             { L _ T.Tselect }
 'vaarg'             { L _ T.Tvaarg }
 'extractelement'             { L _ T.Textractelement }
 'insertelement'             { L _ T.Tinsertelement }
 'shufflevector'             { L _ T.Tshufflevector }
 'extractvalue'             { L _ T.Textractvalue }
 'insertvalue'             { L _ T.Tinsertvalue }
 'landingpad'             { L _ T.Tlandingpad }
 'eq'             { L _ T.Teq }
 'ne'             { L _ T.Tne }
 'ugt'             { L _ T.Tugt }
 'uge'             { L _ T.Tuge }
 'ult'             { L _ T.Tult }
 'sgt'             { L _ T.Tsgt }
 'sge'             { L _ T.Tsge }
 'slt'             { L _ T.Tslt }
 'sle'             { L _ T.Tsle }
 'label'             { L _ T.Tlabel }
 'inbounds'             { L _ T.Tinbounds }
 'align'             { L _ T.Talign }
 'nnan'             { L _ T.Tnnan }
 'ninf'             { L _ T.Tninf }
 'nsz'             { L _ T.Tnsz }
 'arcp'             { L _ T.Tarcp }
 'fast'             { L _ T.Tfast }
 'to'             { L _ T.Tto }
 'nsw'             { L _ T.Tnsw }
 'nuw'             { L _ T.Tnuw }
 'target'             { L _ T.Ttarget }
 'datalayout'             { L _ T.Tdatalayout }
 'triple'             { L _ T.Ttriple }
 'define'             { L _ T.Tdefine }
 'float'             { L _ T.Tfloat }
 'double'             { L _ T.Tdouble }
 INTEGERTYPE             { L _ (T.TintegerType $$) }
 'zeroext'             { L _ T.Tzeroext }
 'signext'             { L _ T.Tsignext }
 'inreg'             { L _ T.Tinreg }
 'byval'             { L _ T.Tbyval }
 'inalloca'             { L _ T.Tinalloca }
 'sret'             { L _ T.Tsret }
 'noalias'             { L _ T.Tnoalias }
 'nocapture'             { L _ T.Tnocapture }
 'nest'             { L _ T.Tnest }
 'returned'             { L _ T.Treturned }
 'readonly'             { L _ T.Treadonly }
 'nounwind'             { L _ T.Tnounwind }
 'uwtable'             { L _ T.Tuwtable }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseConstant     constant
%name parseNamed        namedI
%name parseModule       module

%%

{------------------------------------------------------------------------------
 -
 - Constants
 -
 -----------------------------------------------------------------------------}

constant :: { A.Type -> A.Constant }
constant :
    INT        { intConstant $1 }
  | FLOAT      { floatConstant $1 }

{------------------------------------------------------------------------------
 -
 - Operands
 -
 -----------------------------------------------------------------------------}

operand :: { A.Type -> A.Operand }
operand :
    constant            { A.ConstantOperand . $1 }
  | name                { \_ -> A.LocalReference $1 }

{------------------------------------------------------------------------------
 -
 - Instructions
 -
 -----------------------------------------------------------------------------}

name :: { A.Name }
name :
    NAMED_LOCAL     { A.Name $1 }
  | UNNAMED_LOCAL   { A.UnName $1 }

namedI :: { A.Named A.Instruction }
namedI :
    instruction                     { A.Do $1 }
  | NAMED_LOCAL  '=' instruction    { A.Name $1 A.:= $3 }
  | UNNAMED_LOCAL '=' instruction   { A.UnName $1 A.:= $3 }

intP :: { A.IntegerPredicate }
intP :
   'eq'              { A.EQ }
 | 'ne'              { A.NE }
 | 'ugt'             { A.UGT }
 | 'uge'             { A.UGE }
 | 'ult'             { A.ULT }
 | 'sgt'             { A.SGT }
 | 'sge'             { A.SGE }
 | 'slt'             { A.SLT }
 | 'sle'             { A.SLE }

phiItem :: { A.Type -> (A.Operand, A.Name) }
phiItem :
    '[' operand ',' name ']'     { \t -> ($2 t, $4) }

phiList :: { A.Type -> [(A.Operand, A.Name)] }
phiList :
    phiItem                { \t -> [$1 t] }
  | phiItem ',' phiList    { \t -> $1 t : $3 t }

instruction :: { A.Instruction }
instruction :
    'add' type operand ',' operand         { A.Add False False ($3 $2) ($5 $2) [] }
  | 'fadd' type operand ',' operand        { A.FAdd ($3 $2) ($5 $2) [] }
  | 'sub' type operand ',' operand         { A.Sub False False ($3 $2) ($5 $2) [] }
  | 'fsub' type operand ',' operand        { A.FSub ($3 $2) ($5 $2) [] }
  | 'mul' type operand ',' operand         { A.Mul False False ($3 $2) ($5 $2) [] }
  | 'fmul' type operand ',' operand        { A.FMul ($3 $2) ($5 $2) [] }
  | 'udiv' type operand ',' operand        { A.UDiv False ($3 $2) ($5 $2) [] }
  | 'sdiv' type operand ',' operand        { A.SDiv False ($3 $2) ($5 $2) [] }
  | 'fdiv' type operand ',' operand        { A.FDiv ($3 $2) ($5 $2) [] }
  | 'urem' type operand ',' operand        { A.URem ($3 $2) ($5 $2) [] }
  | 'srem' type operand ',' operand        { A.SRem ($3 $2) ($5 $2) [] }
  | 'frem' type operand ',' operand        { A.FRem ($3 $2) ($5 $2) [] }
  | 'shl' type operand ',' operand         { A.Shl False False ($3 $2) ($5 $2) [] }
  | 'lshr' type operand ',' operand        { A.LShr False ($3 $2) ($5 $2) [] }
  | 'ashr' type operand ',' operand        { A.AShr False ($3 $2) ($5 $2) [] }
  | 'and' type operand ',' operand         { A.And ($3 $2) ($5 $2) [] }
  | 'or' type operand ',' operand          { A.Or ($3 $2) ($5 $2) [] }
  | 'xor' type operand ',' operand         { A.Xor ($3 $2) ($5 $2) [] }
  | 'icmp' intP type operand ',' operand   { A.ICmp $2 ($4 $3) ($6 $3) [] }
  | 'phi' type phiList                     { A.Phi $2 ($3 $2) [] }


instructions :: { [A.Named A.Instruction] } 
instructions :
    {- empty -}                   { [] }
  | namedI instructions           { $1 : $2 }

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

{------------------------------------------------------------------------------
 -
 - Basic Blocks
 -
 -----------------------------------------------------------------------------}

basicBlock :: { A.BasicBlock }
basicBlock :
    JUMPLABEL instructions namedT     { A.BasicBlock (A.Name $1) $2 $3 }
  | instructions namedT               { A.BasicBlock (A.UnName 0) $1 $2 }

basicBlocks :: { [A.BasicBlock] }
basicBlocks :
    {- empty -}             { [] }
  | basicBlock basicBlocks  { $1 : $2 }

{------------------------------------------------------------------------------
 -
 - Global Definitions
 -
 -----------------------------------------------------------------------------}

globalName :: { A.Name }
globalName :
    NAMED_GLOBAL     { A.Name $1 }
  | UNNAMED_GLOBAL   { A.UnName $1 }

type :: { A.Type }
type :
    INTEGERTYPE         { A.IntegerType $1 }
  | 'double'            { A.FloatingPointType 64 A.IEEE }
  | 'float'             { A.FloatingPointType 32 A.IEEE }
  | type '*'            { A.PointerType $1 (A.AddrSpace 0) }

pAttribute :: { A.ParameterAttribute }
pAttribute :
    'nocapture'         { A.NoCapture }

pAttributes :: { [A.ParameterAttribute] }
pAttributes :
    {- empty -}                { [] }
  | pAttribute pAttributes     { $1 : $2 }

parameter :: { A.Parameter }
parameter :
    type pAttributes name         { A.Parameter $1 $3 $2 }

parameterList :: { [A.Parameter] }
parameterList :
    {- empty -}                      { [] }
  | parameter                        { [$1] }
  | parameter ',' parameterList     { $1 : $3 }
    

global :: { A.Global }
global :
    'define' type globalName '(' parameterList ')' '{' basicBlocks '}'
      { A.Function A.External A.Default A.C [] $2 $3 ([], False) [] Nothing 0 Nothing $8 }

{------------------------------------------------------------------------------
 -
 - Definitions
 -
 -----------------------------------------------------------------------------}

definition :: { A.Definition }
definition :
    global         { A.GlobalDefinition $1 }

definitions :: { [A.Definition] }
definitions :
    {- empty -}             { [] }
  | definitions definition  { $2 : $1 }

{------------------------------------------------------------------------------
 -
 - Modules
 -
 -----------------------------------------------------------------------------}

dataLayout :: { Maybe A.DataLayout }
dataLayout :
    {- empty -}                      { Nothing }
  | 'target' 'datalayout' '=' STRING { Nothing --TODO }

targetTriple :: { Maybe String }
targetTriple :
    {- empty -}                       { Nothing }
  | 'target' 'triple' '=' STRING      { Just $4 } 

module :: { A.Module }
module :
    dataLayout targetTriple definitions  { A.Module "" $1 $2 $3 }

{
intConstant :: Integer -> A.Type -> A.Constant
intConstant n (A.IntegerType bs) = A.Int bs n

floatConstant :: Rational -> A.Type -> A.Constant
floatConstant x (A.FloatingPointType 32 _) = A.Float (A.Single (fromRational $2))


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