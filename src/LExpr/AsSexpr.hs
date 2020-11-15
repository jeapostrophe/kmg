module LExpr.AsSexpr (asSexpr) where

import qualified Data.Text as T
import LExpr.AST

class AsSexpr a where
  asSexpr :: a -> T.Text

instance AsSexpr LTFollow where
  asSexpr = \case
    LTFNone _ -> ""
    LTFEscape _ u t -> "(#%escape " <> asSexpr u <> " " <> asSexpr t <> ")"

instance AsSexpr LText where
  asSexpr = \case
    LTExtent _ t f -> "(#%text \"" <> t <> "\" " <> asSexpr f <> ")"

instance AsSexpr LGroupType where
  asSexpr = \case
    LG_Paren -> "#%paren"
    LG_Bracket -> "#%bracket"

instance AsSexpr LUnit where
  asSexpr = \case
    LUVar _ t -> t
    LUOp _ t -> t
    LUNum _ t -> t
    LUGroup _ g us -> "(" <> asSexpr g <> " " <> T.unwords (map asSexpr us) <> ")"
    LUText _ t -> asSexpr t

instance AsSexpr LLFollow where
  asSexpr = \case
    LLFNone _ -> ""
    LLFColon _ p -> "(%colon " <> asSexpr p <> ")"
    LLFComment a t -> asSexpr $ LLComment a t

instance AsSexpr LLine where
  asSexpr = \case
    LLComment _ t ->
      "(%comment \"" <> t <> "\")"
    LLUnits _ us f ->
      "(%units " <> T.unwords (map asSexpr us) <> " " <> asSexpr f <> ")"

instance AsSexpr LPara where
  asSexpr (LPara _ lls) = "(%para " <> T.unwords (map asSexpr lls) <> ")"

instance AsSexpr LDoc where
  asSexpr (LDoc _ ps) = "(#%doc " <> T.unwords (map asSexpr ps) <> ")"
