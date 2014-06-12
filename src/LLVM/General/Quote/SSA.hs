{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module LLVM.General.Quote.SSA ( toSSA ) where

import Prelude hiding (pred)

import LLVM.General.AST

import Control.Monad
import qualified Data.Map.Strict as M
import Data.List

import Control.Monad.ST.Strict
import Data.STRef

import Data.Generics


{-

typeOfConstant :: C.Constant -> Type
typeOfConstant C.Int{..} = IntegerType integerBits
typeOfConstant C.Float{..} = case floatValue of
  _ -> undefined
typeOfConstant C.Null{..} = constantType
typeOfConstant C.Struct{..} =
  StructureType isPacked (map typeOfConstant memberValues)
typeOfConstant C.Array{..} = ArrayType (genericLength memberValues) memberType
typeOfConstant C.Vector{..} = case memberValues of
  (x:_) -> VectorType (genericLength memberValues) (typeOfConstant x)
  [] -> error "typeOfConstant: empty Vector"
typeOfConstant C.Undef{..} = constantType
typeOfConstant C.BlockAddress{} = PointerType (IntegerType 8) (AddrSpace 0)
typeOfConstant (C.GlobalReference t _) = t
typeOfConstant C.Add{..} = typeOfConstant operand0
typeOfConstant C.FAdd{..} = typeOfConstant operand0
typeOfConstant C.Sub{..} = typeOfConstant operand0
typeOfConstant C.FSub{..} = typeOfConstant operand0
typeOfConstant C.Mul{..} = typeOfConstant operand0
typeOfConstant C.FMul{..} = typeOfConstant operand0
typeOfConstant C.UDiv{..} = typeOfConstant operand0
typeOfConstant C.SDiv{..} = typeOfConstant operand0
typeOfConstant C.FDiv{..} = typeOfConstant operand0
typeOfConstant C.URem{..} = typeOfConstant operand0
typeOfConstant C.SRem{..} = typeOfConstant operand0
typeOfConstant C.FRem{..} = typeOfConstant operand0
typeOfConstant C.Shl{..} = typeOfConstant operand0
typeOfConstant C.LShr{..} = typeOfConstant operand0
typeOfConstant C.AShr{..} = typeOfConstant operand0
typeOfConstant C.And{..} = typeOfConstant operand0
typeOfConstant C.Or{..} = typeOfConstant operand0
typeOfConstant C.Xor{..} = typeOfConstant operand0
typeOfConstant C.GetElementPtr{..} = typeOfConstant address
typeOfConstant C.Trunc{..} = type'
typeOfConstant C.ZExt{..} = type'
typeOfConstant C.SExt{..} = type'
typeOfConstant C.FPToUI{..} = type'
typeOfConstant C.FPToSI{..} = type'
typeOfConstant C.UIToFP{..} = type'
typeOfConstant C.SIToFP{..} = type'
typeOfConstant C.FPTrunc{..} = type'
typeOfConstant C.FPExt{..} = type'
typeOfConstant C.PtrToInt{..} = type'
typeOfConstant C.IntToPtr{..} = type'
typeOfConstant C.BitCast{..} = type'
typeOfConstant C.ICmp{..} = case typeOfConstant operand0 of
  VectorType{..} -> VectorType nVectorElements (IntegerType 1)
  _              -> IntegerType 1
typeOfConstant C.FCmp{..} = case typeOfConstant operand0 of
  VectorType{..} -> VectorType nVectorElements (IntegerType 1)
  _              -> IntegerType 1
typeOfConstant C.Select{..} = typeOfConstant trueValue
typeOfConstant C.ExtractElement{..} = case C.memberValues vector of
  (x:_) -> typeOfConstant x
  [] -> error "typeOfConstant: empty Vector"
typeOfConstant C.InsertElement{..} = typeOfConstant vector
typeOfConstant C.ShuffleVector{..} =
  case (typeOfConstant operand0, typeOfConstant mask) of
    (VectorType _ t, VectorType n _) -> VectorType n t
    _ -> error "typeOfConstant: expected vector arguments to ShuffleVector"
typeOfConstant C.ExtractValue{..} =
  extractTypes indices' (typeOfConstant aggregate)
 where
  extractTypes []     t = t
  extractTypes (n:ns) t = case t of
    StructureType{..} -> extractTypes ns (elementTypes !! fromIntegral n)
    ArrayType{..}     -> extractTypes ns elementType
    _                 -> error "typeOfConstant: expected aggregate value in ExtractValue"
typeOfConstant C.InsertValue{..} = typeOfConstant aggregate

typeOfOperand :: Operand -> Type
typeOfOperand (LocalReference t _) = t
typeOfOperand (ConstantOperand c) = typeOfConstant c
typeOfOperand MetadataStringOperand{} = MetadataType
typeOfOperand MetadataNodeOperand{} = MetadataType

-}

type CFG s =[(Name, MutableBlock s)]

data MutableBlock s = MutableBlock {
  blockName :: Name,
  blockIncompletePhis :: STRef s (M.Map Name (MutableInstruction s)),
  blockPhis :: STRef s [MutableInstruction s],
  blockInstructions :: [MutableInstruction s],
  blockTerminator :: MutableTerminator s,
  blockPreds :: [Name],
  blockDefs :: STRef s (M.Map Name Name)
  }

type MutableInstruction s = STRef s (Named Instruction)
type MutableTerminator  s = STRef s (Named Terminator)



toCFG :: [BasicBlock] -> ST s (CFG s)
toCFG blocks =
  flip mapM blocks $ \b@(BasicBlock n _ _) -> do
    b' <- toMutableBlock b $ froms blocks b
    return $ (n,b')

toMutableBlock :: BasicBlock -> [Name] -> ST s (MutableBlock s)
toMutableBlock (BasicBlock n is t) preds = do
  let (phis, is') = partition isPhi is
  phis' <- mapM newSTRef phis >>= newSTRef
  incompletePhis <- newSTRef M.empty
  is'' <- mapM newSTRef is'
  t'  <- newSTRef t
  defs <- newSTRef M.empty
  return $ MutableBlock{
    blockName = n,
    blockIncompletePhis = incompletePhis,
    blockPhis = phis',
    blockInstructions = is'',
    blockTerminator = t',
    blockPreds = preds,
    blockDefs = defs
    }

unNamed :: Named a -> (Maybe Name, a)
unNamed (Do x)   = (Nothing, x)
unNamed (n := x) = (Just n, x)

named :: (Maybe Name, a) -> Named a
named (Nothing, x) = Do x
named (Just n, x)  = n := x

termDests :: Terminator -> [Name]
termDests Ret{}          = []
termDests CondBr{..}     = [trueDest, falseDest]
termDests Br{..}         = [dest]
termDests Switch{..}     = [ d | (_,d) <- dests ]
termDests IndirectBr{..} = possibleDests
termDests Invoke{..}     = [returnDest, exceptionDest]
termDests Resume{}       = []
termDests Unreachable{}  = []

froms :: [BasicBlock] -> BasicBlock -> [Name]
froms blocks (BasicBlock n _ _) = do
  BasicBlock n' _ t <- blocks
  let (_, t') = unNamed t
  d <- termDests t'
  guard $ n==d
  return n'

isPhi :: Named Instruction -> Bool
isPhi instr = case unNamed instr of
  (_, Phi{}) -> True
  _          -> False

fromCFG :: CFG s -> ST s [BasicBlock]
fromCFG cfg = do
  mapM fromMutableBlock $ map snd cfg

fromMutableBlock :: MutableBlock s -> ST s BasicBlock
fromMutableBlock MutableBlock{..} = do
  incompletePhis <- readSTRef blockIncompletePhis
  when (not $ M.null incompletePhis) (error "internal error: incomplete phi nodes")
  phis <- readSTRef blockPhis
  phis' <- mapM readSTRef phis
  is <- mapM readSTRef blockInstructions
  t <- readSTRef blockTerminator
  return $ BasicBlock blockName (phis'++is) t

writeVariable
  :: Name                -- ^ Name of the Variable
  -> Name                -- ^ The new Name
  -> MutableBlock s      -- ^ The Block being updated
  -> ST s ()
writeVariable var varNew MutableBlock{..} =
  modifySTRef' blockDefs $ M.insert var varNew

readVariable
  :: Name                -- ^ Name of the Variable
  -> Type                -- ^ Type of the Variable
  -> MutableBlock s      -- ^ The Block being updated
  -> STRef s Int
  -> ST s Name
readVariable var ty block@MutableBlock{..} ctr = do
  defs' <- readSTRef blockDefs
  case M.lookup var defs' of
    -- local value numbering
    Just n  -> return n
    -- global value numbering
    Nothing ->
      case blockPreds of
        -- entry block, name is function parameter
        []     -> do writeVariable var var block
                     return var
        preds  -> do
          varNew <- freshName ctr var
          phi <- newSTRef $ varNew := Phi ty (map (undefined,) preds) []
          modifySTRef' blockIncompletePhis (M.insert var phi)
          writeVariable var varNew block
          return varNew

freshName :: STRef s Int -> Name -> ST s Name
freshName ctr baseName = do
  i <- readSTRef ctr
  writeSTRef ctr (i+1)
  let nameString = case baseName of
        Name n -> n
        UnName n -> "v" ++ show n
  return $ Name $ nameString ++ "." ++ show i

addPhiOperands
  :: Name
  -> STRef s (Named Instruction)
  -> CFG s
  -> STRef s Int
  -> ST s ()
addPhiOperands var phi cfg ctr = do
  phi' <- readSTRef phi
  let (n, phi'') = unNamed phi'
      preds = map snd $ incomingValues phi''
      ty = type' phi''
  incoming <- flip mapM preds $ \pred -> do
    let Just block = lookup pred cfg
    phiOp <- readVariable var ty block ctr
    return (LocalReference ty phiOp, pred)
  let phi''' = named (n, phi''{incomingValues = incoming})
  writeSTRef phi phi'''

toSSA :: [BasicBlock] -> [BasicBlock]
toSSA bbs = runST $ do
  cfg <- toCFG bbs
  ctr <- newSTRef 1
  mapM_ (blockToSSAPre ctr) (map snd cfg)
  mapM_ (blockToSSAPhi ctr cfg) (map snd cfg)
  handleIncompletePhis ctr cfg
  fromCFG cfg

handleIncompletePhis
  :: STRef s Int
  -> CFG s
  -> ST s ()
handleIncompletePhis ctr cfg = do
  changed <- mapM (handleIncompletePhis' ctr cfg) (map snd cfg)
  case or changed of
    True -> handleIncompletePhis ctr cfg
    False -> return ()

handleIncompletePhis'
  :: STRef s Int
  -> CFG s
  -> MutableBlock s
  -> ST s Bool
handleIncompletePhis' ctr cfg MutableBlock{..} = do
  incompletePhis <- readSTRef blockIncompletePhis
  case M.null incompletePhis of
    True ->
      return False
    False -> do
      phis <- readSTRef blockPhis
      writeSTRef blockIncompletePhis M.empty
      let incompletePhis' = M.toList incompletePhis
      flip mapM_ incompletePhis' $ \(n, phi) -> addPhiOperands n phi cfg ctr
      let phis' = map snd incompletePhis' ++ phis
      writeSTRef blockPhis phis'
      return True

blockToSSAPhi
  :: STRef s Int
  -> CFG s
  -> MutableBlock s
  -> ST s ()
blockToSSAPhi ctr cfg MutableBlock{..} = do
  -- replace names in Phi with correct references
  phis <- readSTRef blockPhis
  flip mapM_ phis $ \phi -> do
    phi' <- readSTRef phi
    let (n, phi''@Phi{..}) = unNamed phi'
    incomingValues' <- flip mapM incomingValues $ \(op,pred) ->
      case op of
        LocalReference ty var -> do
          let Just blockPred = lookup pred cfg
          phiOp <- readVariable var ty blockPred ctr
          return (LocalReference ty phiOp, pred)
        _ -> return (op,pred)
    let completePhi = named (n, phi''{incomingValues = incomingValues'})
    writeSTRef phi completePhi

blockToSSAPre
  :: STRef s Int
  -> MutableBlock s
  -> ST s ()
blockToSSAPre ctr block@MutableBlock{..} = do
  phis <- readSTRef blockPhis
  mapM_ (namedToSSA ctr block) phis
  mapM_ (namedToSSA ctr block) blockInstructions
  namedToSSA ctr block blockTerminator


namedToSSA
  :: forall s a.
     Data a
  => STRef s Int
  -> MutableBlock s
  -> STRef s (Named a)
  -> ST s ()
namedToSSA ctr block@MutableBlock{..} x = do
  x' <- readSTRef x
  let (n,instr) = unNamed x'
  n' <- maybe (return Nothing) (\n' -> Just `fmap` freshName ctr n') n
  case cast instr of
    Just Phi{} -> writeSTRef x $ named (n', instr)
    _ -> do
      instr' <- trans instr
      writeSTRef x $ named (n', instr')
  case (n,n') of
    (Just n1, Just n1') -> writeVariable n1 n1' block
    (_      , _       ) -> return ()
 where
  trans :: (Data d) => d -> ST s d
  trans = gmapM trans `extM` rename

  rename :: Operand -> ST s (Operand)
  rename (LocalReference t n) = do
    n' <- readVariable n t block ctr
    return $ LocalReference t n'
  rename op = return op
