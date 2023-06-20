module Stepper.Parser.Token
  ( Qualifier(..),
    ConstrOrVariable(..),
    Token(..)
  ) where

import Data.Text (Text)
import Data.IText (IText)
import Numeric.Natural (Natural)

data ConstrOrVariable =
    ConstrName
  | VariableName
  deriving (Eq, Ord, Show)

data Qualifier =
    NoQualifier
  | ModQualifier IText
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
  | TokenIdent Qualifier ConstrOrVariable IText
  | TokenOpIdent Qualifier ConstrOrVariable IText
  | TokenPrimOp IText IText
  | TokenEOF
  deriving (Eq, Ord, Show)