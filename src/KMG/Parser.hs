module KMG.Parser (parseFile) where

import Control.Monad
import Control.Monad.Reader
--import Control.Applicative hiding (some, many)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import KMG.AST

data Env = Env
  { env_ilvl :: Int }

addIndent :: Parser a -> Parser a
addIndent m = do
  Env {..} <- ask
  local (\env -> env { env_ilvl = 1 + env_ilvl }) $
    m

checkIndent :: Parser ()
checkIndent = do
  Env {..} <- ask
  void $ count (2*env_ilvl) (single ' ')
  return ()

type Parser = ParsecT Void Text (ReaderT Env IO)

srcloc :: Parser SrcLoc
srcloc = do
  SourcePos {..} <- getSourcePos
  return $ SrcLoc sourceName (unPos sourceLine) (unPos sourceColumn)

pl_comment :: Parser KLine
pl_comment = label "line comment" $ do
  void $ chunk ";"
  KLComment <$> srcloc <*> takeWhileP (Just "not nl") (/= '\n') <* newline

pt :: Parser Char -> Parser Char -> Parser Text
pt start middle = T.pack <$> ((:) <$> start <*> many middle)

pt_var :: Parser Text
pt_var = pt letterChar alphaNumChar

pt_op :: Parser Text
pt_op = pt symbolChar symbolChar

pu_var :: Parser KUnit
pu_var = label "variable" $ do
  KUVar <$> srcloc <*> (pt_var <|> pt_op) <* space

punit :: Parser KUnit
punit = label "unit" $ do
  pu_var

plf_none :: Parser KLFollow
plf_none = do
  void $ newline
  KLFNone <$> srcloc

plf_colon :: Parser KLFollow
plf_colon = do
  void $ chunk ":"
  KLFColon <$> srcloc <*> addIndent pdoc

plfollow :: Parser KLFollow
plfollow = label "line follower" $ do
  plf_none
  <|> plf_colon

pl_units :: Parser KLine
pl_units = label "units" $ do
  KLUnits <$> srcloc <*> many punit <*> plfollow

pline :: Parser KLine
pline = label "line" $ checkIndent *>
  pl_comment
  <|> pl_units

ppara :: Parser KPara
ppara = label "paragraph" $
  KPara <$> srcloc <*> many pline <* newline

pdoc :: Parser KDoc
pdoc = label "document" $ 
  KDoc <$> srcloc <*> many ppara <* newline

pfile :: Parser KDoc
pfile = do
  void $ chunk "#lang kmg\n"
  pdoc <* eof

parseFile :: FilePath -> IO KDoc
parseFile f = do
  t <- TIO.readFile f
  let env_ilvl = 0 :: Int
  let env = Env {..}
  r <- flip runReaderT env $ runParserT pfile f t
  case r of
    Left peb -> do
      putStr $ errorBundlePretty peb
      error $ "parse error"
    Right d ->
      return d
