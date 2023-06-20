module Stepper.Evaluator where

import Stepper.Syntax.Scoped

evalstep :: Module -> Maybe Module  -- Nothing <=> nothing to reduce
evalstep (Mod bs) = Just (Mod bs)