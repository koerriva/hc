{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.FilePath
import Language.Java.Parser
import Language.Java.Syntax
import Lang.Emit
import Lang.JIT
import Lang.Misc

main :: IO ()
main = do
  args <- getArgs
  if length args == 0 then
    putStrLn "Usage:hc input-file"
  else
    do
      let filepath = args!!0
      file <- readFile filepath
      case parser compilationUnit file of
        Left err -> (print . show) err
        Right r -> do
          print r
          compile filepath r

compile :: FilePath -> CompilationUnit -> IO ()
compile filepath unit = do
  let filename = takeFileName filepath
  putStrLn filename
  ast <- codegen filename unit
  runJIT ast "main"
--  dumpOptObj' ast
--  dumpObj ast "x86_64-pc-none-elf" "x86-64" [] relocaModel codeModel codeOptLvl
--    where
--      relocaModel = readRelocationModel "Static"
--      codeModel = readCodeModel "Large"
--      codeOptLvl = readCodeGenOptModel "None"