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
    "Integer",
    "Char",
    "IO",
    "Prelude",
    "()"    { alias = "_tup0" },
    "(,)"   { alias = "_tup2" },
    "(,,)"  { alias = "_tup3" },
    "(,,,)" { alias = "_tup4" },
    ">>="   { alias = "_bind"},
    ">>"    { alias = "_bind_ignore" },
    "+"     { alias = "_plus" },
    "-"     { alias = "_minus" },
    "\\"    { alias = "_backslash" },
    "_"     { alias = "_underscore" }
  ]