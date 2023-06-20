module Stepper.Syntax.Basic where

import Numeric.Natural
import Data.Text
import Data.IText

import Stepper.BuiltIn

data Lit =
    NatL Natural
  | IntL Integer
  | FrcL Rational
  | StrL Text
  | ChrL Char
  deriving (Eq, Show)

data PrimOp =
    Natural_add
  | Natural_sub
  | Natural_mul
  | Natural_div
  | Natural_eq
  | Integer_add
  | Integer_sub
  | Integer_mul
  | Integer_div
  | Integer_eq
  | Fractional_add
  | Fractional_sub
  | Fractional_mul
  | Fractional_div
  | Fractional_eq
  -- | IO_putStr
  -- | IO_getLine
  -- | IO_bind
  deriving (Eq, Show, Enum, Bounded)

primopName :: PrimOp -> (IText, IText)
primopName p =
  case p of
    Natural_add -> (builtInStrings._Natural, builtInStrings._add)
    Natural_sub -> (builtInStrings._Natural, builtInStrings._sub)
    Natural_mul -> (builtInStrings._Natural, builtInStrings._mul)
    Natural_div -> (builtInStrings._Natural, builtInStrings._div)
    Natural_eq  -> (builtInStrings._Natural, builtInStrings._eq)
    Integer_add -> (builtInStrings._Integer, builtInStrings._add)
    Integer_sub -> (builtInStrings._Integer, builtInStrings._sub)
    Integer_mul -> (builtInStrings._Integer, builtInStrings._mul)
    Integer_div -> (builtInStrings._Integer, builtInStrings._div)
    Integer_eq  -> (builtInStrings._Integer, builtInStrings._eq)
    Fractional_add -> (builtInStrings._Fractional, builtInStrings._add)
    Fractional_sub -> (builtInStrings._Fractional, builtInStrings._sub)
    Fractional_mul -> (builtInStrings._Fractional, builtInStrings._mul)
    Fractional_div -> (builtInStrings._Fractional, builtInStrings._div)
    Fractional_eq  -> (builtInStrings._Fractional, builtInStrings._eq)