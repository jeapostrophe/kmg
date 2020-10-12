module KMG.Compiler (compiler) where

import System.Environment
import System.Exit
import KMG.Parser
import Text.Pretty.Simple (pPrint)

compileFile :: String -> IO ()
compileFile f = do
  ast <- parseFile f
  pPrint ast
  putStrLn $ "XXX"

compiler :: IO ()
compiler = do
  args <- getArgs
  case args of
    [ f ] -> compileFile f
    _ -> do
      putStrLn "expected one source file on command-line"
      exitWith $ ExitFailure 1
