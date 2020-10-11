{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module KMG.AST where

import Data.Text (Text)

data SrcOrigin
  = SOFile FilePath
  deriving (Eq, Ord, Show)

data SrcLoc
  = SrcLoc FilePath Int Int
  deriving (Eq, Show)

data KUnit
  = KUVar SrcLoc Text
  deriving (Eq, Show)

data KLFollow
  = KLFNone SrcLoc
  | KLFColon SrcLoc KDoc
  deriving (Eq, Show)

data KLine
  = KLComment SrcLoc Text
  | KLUnits SrcLoc [KUnit] KLFollow
  deriving (Eq, Show)

data KPara
  = KPara SrcLoc [KLine]
  deriving (Eq, Show)

data KDoc
  = KDoc SrcLoc [KPara]
  deriving (Eq, Show)

-- I want to do something like Lexprs
-- https://github.com/racket/rhombus-brainstorming/blob/e7202afb9615fa722b3d01cb652515b11d047a00/lexpr/0004-lexpr.md
-- https://github.com/racket/rhombus-brainstorming/blob/0d53773661e08e348487a83b77adf2abf154b62b/sapling/0005-sapling.md
-- But refined a little bit. The idea is that a program is a "document" and a
-- document is a sequence of paragraphs, which are sequences of lines. Lines may
-- embed documents via indenting signalled by something like :. So far, this
-- might be the "reader", not the "parser" AST.
