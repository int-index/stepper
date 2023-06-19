{-# LANGUAGE RecordWildCards #-}

module Stepper.BuiltIn
  ( BuiltInStrings(..),
    baseStringPool,
    builtInStrings,
  ) where

import qualified Data.IText as IText
import Data.IText (ITextPool)

import Stepper.BuiltIn.Strings
    ( internBuiltInStrings, BuiltInStrings(..) )

builtInStrings :: BuiltInStrings
baseStringPool :: ITextPool
(builtInStrings, baseStringPool) = internBuiltInStrings IText.emptyPool