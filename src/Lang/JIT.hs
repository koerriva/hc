module Lang.JIT where

import Control.Monad.State.Strict
import qualified Language.Java.Syntax as S
import qualified LLVM.AST as AST

import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import LLVM.PassManager
import LLVM.Transforms
import LLVM.Target
import LLVM.Relocation
import LLVM.CodeModel
import LLVM.CodeGenOpt
import qualified LLVM.ExecutionEngine as EE
import Foreign.Ptr ( FunPtr, castFunPtr )

import qualified Data.Map as Map

import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BC
import Lang.Misc
import Debug.Trace

import System.FilePath

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

readRelocationModel string = read string :: LLVM.Relocation.Model
readCodeModel string = read string :: LLVM.CodeModel.Model
readCodeGenOptModel string = read string :: LLVM.CodeGenOpt.Level

dumpObj :: AST.Module -> BS.ShortByteString -> BC.ByteString -> [(CPUFeature,Bool)]
  -> LLVM.Relocation.Model -> LLVM.CodeModel.Model -> LLVM.CodeGenOpt.Level -> IO ()
dumpObj mod triple cpu features reloca cmodel lvl = do
  withTargetOptions $ \opts -> do
    (target,s)<- lookupTarget Nothing triple
    traceM $ "lookup target : " ++ show s
    withTargetMachine target triple cpu (toMap features) opts reloca cmodel lvl (dump mod)
    where
      toMap :: [(CPUFeature, Bool)] -> Map.Map CPUFeature Bool
      toMap = foldl (\seek (k,v) -> Map.insert k v seek) Map.empty

dumpOptObj' :: AST.Module -> IO ()
dumpOptObj' mod = do
  withHostTargetMachine $ \targetMachine -> do
    withContext $ \context -> do
      withModuleFromAST context mod $ \m -> do
        withPassManager passes $ \pm -> do
          runPassManager pm m
          optmod <- moduleAST m
          dump optmod targetMachine

dump :: AST.Module -> TargetMachine -> IO ()
dump astMod targetMachine = do
  withContext $ \context -> do
    withModuleFromAST context astMod $ \m -> do
      let modFileName = (File . (++".o") . takeBaseName . toString) $ AST.moduleSourceFileName astMod
      writeObjectToFile targetMachine modFileName m
      putStrLn "dump ok !"

runJIT :: AST.Module -> String -> IO ()
runJIT mod fnName = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          {-runPassManager pm m-}
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.mkName fnName)
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()
          return ()