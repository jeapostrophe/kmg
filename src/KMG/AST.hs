{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module KMG.AST where

data SrcOrigin
  = SOFile FilePath
  deriving (Eq, Ord, Show)

data KText
  = KTString String
  deriving (Eq, Show)

data KPara
  = KPText [KText]
  deriving (Eq, Show)

data KDoc
  = KDoc [KPara]
  deriving (Eq, Show)

-- I want to do something like Lexprs
-- https://github.com/racket/rhombus-brainstorming/pull/114/files?short_path=209689e#diff-209689e2f5de9c64d4ce6e7105569c79
-- But refined a little bit. The idea is that a program is a "document" and a
-- document is a sequence of paragraphs, which are sequences of lines. Lines may
-- embed documents via indenting signalled by something like :. So far, this
-- might be the "reader", not the "parser" AST.
