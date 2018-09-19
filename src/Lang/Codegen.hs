{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State hiding (void)
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Typed
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

import Lang.Misc

import Debug.Trace

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

type ModuleContext = Map.Map AST.Name AST.Type

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: FilePath -> AST.Module
emptyModule filename = defaultModule {
  moduleName = ""
  , moduleSourceFileName = toShortBS filename
  }

renameModule :: String -> LLVM ()
renameModule label = modify $ \s -> s {moduleName = toShortBS label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = mkName label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = mkName label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (mkName entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Type -> Instruction -> Codegen (Operand)
instr retTy ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
--  traceM $ "inster : " ++ show retTy ++ ", " ++ show ins
  return $ local retTy ref

instrVoid :: Instruction -> Codegen ()
instrVoid ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (Do ins) : i } )
  return ()

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = Map.insert (mkName qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (mkName qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local ::  Type -> Name -> Operand
local = LocalReference

global ::  Type -> Name -> C.Constant
global = C.GlobalReference

externf :: Type -> Name -> Operand
externf ty nm = ConstantOperand $ C.GlobalReference ty nm

-- Arithmetic and Constants
add :: Type -> Operand -> Operand -> Codegen Operand
add ty a b = instr ty $ Add False False a b []

sub :: Type -> Operand -> Operand -> Codegen Operand
sub ty a b = instr ty $ Sub False False a b []

mul :: Type -> Operand -> Operand -> Codegen Operand
mul ty a b = instr ty $ Mul False False a b []

udiv :: Type -> Operand -> Operand -> Codegen Operand
udiv ty a b = instr ty $ UDiv False a b []

sdiv :: Type -> Operand -> Operand -> Codegen Operand
sdiv ty a b = instr ty $ SDiv False a b []

urem :: Type -> Operand -> Operand -> Codegen Operand
urem ty a b = instr ty $ URem a b []

srem :: Type -> Operand -> Operand -> Codegen Operand
srem ty a b = instr ty $ SRem a b []

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr double $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr double $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr double $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr double $ FDiv noFastMathFlags a b []

frem :: Operand -> Operand -> Codegen Operand
frem a b = instr double $ FRem noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr double $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr ty $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects

call :: Operand -> [(Operand, [A.ParameterAttribute])] -> Codegen Operand
call fn args = do
  let ins = Call {
    AST.tailCallKind = Nothing
  , AST.callingConvention = CC.C
  , AST.returnAttributes = []
  , AST.function = Right fn
  , AST.arguments = args
  , AST.functionAttributes = []
  , AST.metadata = []
  }
  case typeOf fn of
      FunctionType r _ _ -> case r of
        VoidType -> instrVoid ins >> (pure (ConstantOperand (C.Undef void)))
        _        -> instr r ins
      PointerType (FunctionType r _ _) _ -> case r of
        VoidType -> instrVoid ins >> (pure (ConstantOperand (C.Undef void)))
        _        -> instr r ins
      _ -> error "Cannot call non-function (Malformed AST)."

alloca :: Type -> Codegen Operand
alloca ty = instr (ptr ty) $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = instrVoid $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load a = instr retty $ Load False a Nothing 0 []
  where
    retty = case typeOf a of
      PointerType ty _ -> ty
      _ -> error "Cannot load non-pointer (Malformed AST)."

extractElement :: Operand -> Operand -> Codegen Operand
extractElement v i = instr elemTyp $ ExtractElement v i []
  where elemTyp =
          case typeOf v of
            VectorType _ typ -> typ
            _ -> error "extractElement: Expected a vector type (malformed AST)."

extractValue :: Operand -> [Word32] -> Codegen Operand
extractValue array idx = instr (extractValueType idx (typeOf array)) $ ExtractValue array idx []

insertValue :: Operand -> Operand -> [Word32] -> Codegen Operand
insertValue array val idx = instr (typeOf array) $ InsertValue array val idx []
-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

--ret :: MonadIRBuilder m => Operand -> m ()
--ret val = emitTerm (Ret (Just val) [])

ret :: Maybe Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret val []