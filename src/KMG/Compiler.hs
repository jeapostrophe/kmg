module KMG.Compiler (compiler) where

import qualified Data.Text.IO as TIO
import LExpr.Parser
import LExpr.AsSexpr
import System.Environment
import System.Exit
-- import Text.Pretty.Simple (pPrint)

compileFile :: String -> IO ()
compileFile f = do
  ast <- parseFile f
  TIO.putStr $ asSexpr ast
  putStrLn $ "XXX"

compiler :: IO ()
compiler = do
  args <- getArgs
  case args of
    [ f ] -> compileFile f
    _ -> do
      putStrLn "expected one source file on command-line"
      exitWith $ ExitFailure 1
