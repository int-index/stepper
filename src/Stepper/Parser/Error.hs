module Stepper.Parser.Error where

import qualified Text.Megaparsec as P
import Stepper.Located
import Stepper.Parser.Token

data PsError =
    PsErrUnexpectedToken (Located Token)
  | PsErrBadIndentation
  | PsErrTodo
  deriving (Eq, Ord)

instance P.ShowErrorComponent PsError where
  showErrorComponent e =
    case e of
      PsErrUnexpectedToken (L loc tok) ->
        "Unexpected token " ++ show tok ++ " at " ++ show loc
      PsErrBadIndentation -> "bad indent"
      PsErrTodo -> "some sort of parse error"