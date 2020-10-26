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

dlabel :: forall a . Show a => String -> Parser a -> Parser a
dlabel lab m = mdbg $ label lab $ m
  where mdbg :: Parser a -> Parser a
        mdbg = if True then dbg lab else id

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

varStartChar :: Parser Char
varStartChar = letterChar

varRestChar :: Parser Char
varRestChar = alphaNumChar

opChar :: Parser Char
opChar = oneOf ("!@#$%^&*-_=+|\\<>/?.:;"::String)

pu_op :: Parser KUnit
pu_op = dlabel "operator" $ do
  KUOp <$> srcloc <*> (notFollowedBy (chunk ":" *> pufollow) *> pt opChar opChar)

pu_var :: Parser KUnit
pu_var = dlabel "variable" $ do
  KUVar <$> srcloc <*> pt varStartChar varRestChar

pu_group_type :: Parser (KGroupType, Char, Char)
pu_group_type =
  (return $ (KG_Paren, '(', ')')) <|>
  (return $ (KG_Bracket, '[', ']'))

pu_group :: Parser KUnit
pu_group = dlabel "group" $ do
  (kgt, gs, ge) <- pu_group_type
  KUGroup <$> srcloc <*> pure kgt <*> between (char gs) (char ge) (some punit)

ptf_none :: Parser KTFollow
ptf_none = do
  void $ chunk "}"
  KTFNone <$> srcloc

ptf_escape :: Parser KTFollow
ptf_escape = dlabel "text escape" $ do
  void $ chunk "@"
  KTFEscape <$> srcloc <*> punit <*> ptext

ptfollow :: Parser KTFollow
ptfollow = dlabel "text follower" $ do
  ptf_none <|> ptf_escape

ptext :: Parser KText
ptext = dlabel "text" $ do
  KTExtent <$> srcloc <*> (T.pack <$> many (noneOf ("@\n}" :: String))) <*> ptfollow

pu_text :: Parser KUnit
pu_text = dlabel "text unit" $ do
  KUText <$> srcloc <*> ((char '{') *> ptext)

pufollow :: Parser ()
pufollow = dlabel "unit follower" $ void $ do
  chunk " "
  <|> lookAhead (chunk "\n")
  <|> lookAhead (chunk ")")
  <|> lookAhead (chunk "}")

punit :: Parser KUnit
punit = dlabel "unit" $ do
  (pu_var <|> pu_op <|> pu_group <|> pu_text) <* pufollow

plf_none :: Parser KLFollow
plf_none = do
  void $ chunk "\n"
  KLFNone <$> srcloc

plf_colon :: Parser KLFollow
plf_colon = do
  void $ chunk ":"
  void $ lookAhead $ chunk "\n"
  KLFColon <$> srcloc <*> addIndent ppara

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
ppara = dlabel "paragraph" $ do
  void $ chunk "\n"
  KPara <$> srcloc <*> some pline

pdoc :: Parser KDoc
pdoc = dlabel "document" $
  KDoc <$> srcloc <*> many ppara

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
