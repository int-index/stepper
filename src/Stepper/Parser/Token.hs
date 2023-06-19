module Stepper.Parser.Token
  ( ConstrOrVariable(..),
    Token(..)
  ) where

import Data.Text (Text)
import Data.IText (IText)
import Numeric.Natural (Natural)

data ConstrOrVariable =
    ConstrName
  | VariableName
  deriving (Eq, Ord, Show)

data Token =
    TokenEq
  | TokenLPar
  | TokenRPar
  | TokenLSqBr
  | TokenRSqBr
  | TokenLCuBr
  | TokenRCuBr
  | TokenStartLayout
  | TokenEndLayout
  | TokenComma
  | TokenSemicolon
  | TokenBackslash
  | TokenArrRight
  | TokenKwLet
  | TokenKwIn
  | TokenKwCase
  | TokenKwOf
  | TokenUnderscore
  | TokenNatLit Natural
  | TokenIntLit Integer
  | TokenFrcLit Rational
  | TokenStrLit Text
  | TokenChrLit Char
  | TokenIdent ConstrOrVariable IText
  | TokenOpIdent ConstrOrVariable IText
  | TokenEOF
  deriving (Eq, Ord, Show)