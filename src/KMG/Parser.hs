module KMG.Parser (parseFile) where

import KMG.AST

parseString :: SrcOrigin -> String -> KDoc
parseString so s =
  error $ "XXX " <> show so <> " " <> show s

parseFile :: FilePath -> IO KDoc
parseFile f = do
  t <- readFile f
  return $ parseString (SOFile f) t
