module Stepper.Located
  ( module Text.Megaparsec.Pos,
    SourceSpan(..),
    mkSourceSpan,
    Located(..),
  ) where

import Text.Megaparsec.Pos

data SourceSpan =
  SourceSpan {
    sourceName :: FilePath,
    sourceLineStart :: Pos,
    sourceColumnStart :: Pos,
    sourceLineEnd :: Pos,
    sourceColumnEnd :: Pos
  }
  deriving (Eq, Ord, Show)

mkSourceSpan :: SourcePos -> SourcePos -> SourceSpan
mkSourceSpan posStart posEnd =
  SourceSpan {
    sourceName = posStart.sourceName,
    sourceLineStart = posStart.sourceLine,
    sourceColumnStart = posStart.sourceColumn,
    sourceLineEnd = posEnd.sourceLine,
    sourceColumnEnd = posEnd.sourceColumn
  }

data Located a =
  L {
    location :: SourceSpan,
    located :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)