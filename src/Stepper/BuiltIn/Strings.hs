{-# LANGUAGE TemplateHaskell #-}

module Stepper.BuiltIn.Strings
  ( BuiltInStrings(..),
    internBuiltInStrings,
  ) where

import Stepper.BuiltIn.GenDecls (genBuiltInStrings, alias)

genBuiltInStrings
  [
    "let",
    "in",
    "case",
    "of",
    "do",
    "where",
    "data",
    "class",
    "type",
    "forall",
    "exists",
    "if",
    "then",
    "else",
    "putStr",
    "putStrLn",
    "Type",
    "String",
    "Natural",
    "Integer",
    "Fractional",
    "Char",
    "IO",
    "Prelude",
    "add",
    "sub",
    "mul",
    "div",
    "eq",
    "()"    { alias = "_tup0" },
    "(,)"   { alias = "_tup2" },
    "(,,)"  { alias = "_tup3" },
    "(,,,)" { alias = "_tup4" },
    ">>="   { alias = "_bind"},
    ">>"    { alias = "_bind_ignore" },
    "->"    { alias = "_arrow_right" },
    "="     { alias = "_equals" },
    "+"     { alias = "_plus" },
    "-"     { alias = "_minus" },
    "\\"    { alias = "_backslash" },
    "_"     { alias = "_underscore" }
  ]