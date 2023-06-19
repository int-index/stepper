module Stepper.Syntax.Literal where

import Numeric.Natural
import Data.Text

data Lit =
    NatL Natural
  | IntL Integer
  | FrcL Rational
  | StrL Text
  | ChrL Char
  deriving (Eq, Show)