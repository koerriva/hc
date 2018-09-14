module Main where

import System.Environment (getArgs)
import Language.Java.Parser
import Language.Java.Syntax

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
        Right r -> print r

compile :: CompilationUnit -> IO ()
compile unit = undefined
