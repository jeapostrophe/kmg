{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module LExpr.AST where

import Data.Text (Text)

data SrcLoc
  = SrcLoc FilePath Int Int
  deriving (Eq, Show)

data LTFollow
  = LTFNone SrcLoc
  | LTFEscape SrcLoc LUnit LText
  deriving (Eq, Show)

data LText
  = LTExtent SrcLoc Text LTFollow
  deriving (Eq, Show)

data LGroupType
  = LG_Paren
  | LG_Bracket
  deriving (Eq, Show)

data LUnit
  = LUVar SrcLoc Text
  | LUOp SrcLoc Text
  | LUNum SrcLoc Text
  | LUGroup SrcLoc LGroupType [LUnit]
  | LUText SrcLoc LText
  deriving (Eq, Show)

data LLFollow
  = LLFNone SrcLoc
  | LLFColon SrcLoc LPara
  | LLFComment SrcLoc Text
  deriving (Eq, Show)

data LLine
  = LLComment SrcLoc Text
  | LLUnits SrcLoc [LUnit] LLFollow
  deriving (Eq, Show)

data LPara
  = LPara SrcLoc [LLine]
  deriving (Eq, Show)

data LDoc
  = LDoc SrcLoc [LPara]
  deriving (Eq, Show)

-- I want to do something like Lexprs
-- https://github.com/racket/rhombus-brainstorming/blob/e7202afb9615fa722b3d01cb652515b11d047a00/lexpr/0004-lexpr.md
-- https://github.com/racket/rhombus-brainstorming/blob/0d53773661e08e348487a83b77adf2abf154b62b/sapling/0005-sapling.md
-- But refined a little bit. The idea is that a program is a "document" and a
-- document is a sequence of paragraphs, which are sequences of lines. Lines may
-- embed documents via indenting signalled by something like :. So far, this
-- might be the "reader", not the "parser" AST.
