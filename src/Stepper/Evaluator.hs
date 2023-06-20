module Stepper.Evaluator where

import Data.IText
import Stepper.Syntax.Scoped

evalstep :: Module -> IText -> Maybe Module  -- Nothing <=> nothing to reduce
evalstep (Mod bs) _entryPoint = Just (Mod bs)