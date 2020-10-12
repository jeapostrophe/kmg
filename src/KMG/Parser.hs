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
import Text.Megaparsec.Debug
import KMG.AST

data Env = Env
  { env_ilvl :: Int }

addIndent :: Parser a -> Parser a
addIndent m = do
  Env {..} <- ask
  local (\env -> env { env_ilvl = 1 + env_ilvl }) $
    m

checkIndent :: Parser a -> Parser a
checkIndent m = do
  Env {..} <- ask
  void $ count (2*env_ilvl) (single ' ')
  m

type Parser = ParsecT Void Text (ReaderT Env IO)

dlabel :: Show a => String -> Parser a -> Parser a
dlabel lab m = dbg lab $ label lab $ m

srcloc :: Parser SrcLoc
srcloc = do
  SourcePos {..} <- getSourcePos
  return $ SrcLoc sourceName (unPos sourceLine) (unPos sourceColumn)

pl_comment :: Parser KLine
pl_comment = dlabel "line comment" $ do
  void $ chunk ";"
  KLComment <$> srcloc <*> takeWhileP (Just "not nl") (/= '\n') <* newline

pt :: Parser Char -> Parser Char -> Parser Text
pt start middle = T.pack <$> ((:) <$> start <*> many middle)

pt_var :: Parser Text
pt_var = pt letterChar alphaNumChar

pt_op :: Parser Text
pt_op = pt symbolChar symbolChar

pu_var :: Parser KUnit
pu_var = dlabel "variable" $ do
  KUVar <$> srcloc <*> (pt_var <|> pt_op) <* space

punit :: Parser KUnit
punit = dlabel "unit" $ do
  pu_var

plf_none :: Parser KLFollow
plf_none = do
  void $ chunk "\n"
  KLFNone <$> srcloc

plf_colon :: Parser KLFollow
plf_colon = do
  void $ chunk ":\n"
  KLFColon <$> srcloc <*> addIndent pdoc

plfollow :: Parser KLFollow
plfollow = dlabel "line follower" $ do
  plf_none
  <|> plf_colon

pl_units :: Parser KLine
pl_units = dlabel "units" $ do
  KLUnits <$> srcloc <*> some punit <*> plfollow

pline :: Parser KLine
pline = dlabel "line" $ checkIndent $
  pl_comment
  <|> pl_units

ppara :: Parser KPara
ppara = dlabel "paragraph" $
  KPara <$> srcloc <*> some pline

pdoc :: Parser KDoc
pdoc = dlabel "document" $
  KDoc <$> srcloc <*> some ppara

pfile :: Parser KDoc
pfile = dlabel "file" $ do
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
