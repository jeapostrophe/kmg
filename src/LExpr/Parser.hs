module LExpr.Parser (parseFile) where

import Control.Monad
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import LExpr.AST

debug :: Bool
debug = False

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
        mdbg = if debug then dbg lab else id

srcloc :: Parser SrcLoc
srcloc = do
  SourcePos {..} <- getSourcePos
  return $ SrcLoc sourceName (unPos sourceLine) (unPos sourceColumn)

pl_comment :: Parser LLine
pl_comment = dlabel "line comment" $ do
  void $ chunk ";"
  LLComment <$> srcloc <*> takeWhileP (Just "not nl") (/= '\n') <* newline

pt :: Parser Char -> Parser Char -> Parser Text
pt start middle = T.pack <$> ((:) <$> start <*> many middle)

varStartChar :: Parser Char
varStartChar = letterChar

varRestChar :: Parser Char
varRestChar = alphaNumChar

opChar :: Parser Char
opChar = oneOf ("`~!@#$%^&*-_=+|\\<>/?,.:;"::String)

pu_op :: Parser LUnit
pu_op = dlabel "operator" $ do
  LUOp <$> srcloc <*> (notFollowedBy (chunk ":" *> pufollow) *> pt opChar opChar)

pu_var :: Parser LUnit
pu_var = dlabel "variable" $ do
  LUVar <$> srcloc <*> pt varStartChar varRestChar

pu_group_type :: Parser (LGroupType, Char, Char)
pu_group_type =
  (return $ (LG_Paren, '(', ')')) <|>
  (return $ (LG_Bracket, '[', ']'))

pu_group :: Parser LUnit
pu_group = dlabel "group" $ do
  (kgt, gs, ge) <- pu_group_type
  LUGroup <$> srcloc <*> pure kgt <*> between (char gs) (char ge) (some punit)

ptf_none :: Parser LTFollow
ptf_none = do
  void $ chunk "}"
  LTFNone <$> srcloc

ptf_escape :: Parser LTFollow
ptf_escape = dlabel "text escape" $ do
  void $ chunk "@"
  LTFEscape <$> srcloc <*> punit <*> ptext

ptfollow :: Parser LTFollow
ptfollow = dlabel "text follower" $ do
  ptf_none <|> ptf_escape

ptext :: Parser LText
ptext = dlabel "text" $ do
  LTExtent <$> srcloc <*> (T.pack <$> many (noneOf ("@\n}" :: String))) <*> ptfollow

pu_text :: Parser LUnit
pu_text = dlabel "text unit" $ do
  LUText <$> srcloc <*> ((char '{') *> ptext)

pufollow :: Parser ()
pufollow = dlabel "unit follower" $ do
  void $ chunk " "
  <|> lookAhead (void $ oneOf ("\n)]}" :: String))

punit :: Parser LUnit
punit = dlabel "unit" $ do
  (pu_var <|> pu_op <|> pu_group <|> pu_text) <* pufollow

plf_none :: Parser LLFollow
plf_none = do
  void $ chunk "\n"
  LLFNone <$> srcloc

plf_colon :: Parser LLFollow
plf_colon = do
  void $ chunk ":"
  void $ lookAhead $ chunk "\n"
  LLFColon <$> srcloc <*> addIndent ppara

plfollow :: Parser LLFollow
plfollow = dlabel "line follower" $ do
  plf_none
  <|> plf_colon

pl_units :: Parser LLine
pl_units = dlabel "units" $ do
  LLUnits <$> srcloc <*> some punit <*> plfollow

pline :: Parser LLine
pline = dlabel "line" $ checkIndent $
  pl_comment
  <|> pl_units

ppara :: Parser LPara
ppara = dlabel "paragraph" $ do
  void $ chunk "\n"
  LPara <$> srcloc <*> some pline

pdoc :: Parser LDoc
pdoc = dlabel "document" $
  LDoc <$> srcloc <*> many ppara

pfile :: Parser LDoc
pfile = dlabel "file" $ do
  void $ chunk "#lang kmg\n"
  pdoc <* eof

parseFile :: FilePath -> IO LDoc
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
