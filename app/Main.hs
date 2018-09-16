{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Language.Java.Parser
import Language.Java.Syntax
import Lang.Emit
import Lang.JIT

main :: IO ()
main = do
  args <- getArgs
  if length args == 0 then
    putStrLn "Usage:hc input-file"
  else
    do
      file <- readFile (args!!0)
      case parser compilationUnit file of
        Left err -> (print . show) err
        Right r -> do
          print r
          compile r

compile :: CompilationUnit -> IO ()
compile unit = do
  ast <- codegen unit
  runJIT ast "test"
  dumpObj ast "x86_64-unknow-standalone" "x86-64" [] relocaModel codeModel codeOptLvl
    where
      relocaModel = readRelocationModel "Static"
      codeModel = readCodeModel "Large"
      codeOptLvl = readCodeGenOptModel "Default"
