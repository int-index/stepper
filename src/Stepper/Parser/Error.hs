module Stepper.Parser.Error where

import qualified Data.Text as Text
import Data.IText
import qualified Text.Megaparsec as P
import Stepper.Located
import Stepper.Parser.Token

data PsError =
    PsErrUnexpectedToken (Located Token)
  | PsErrBadIndentation
  | PsErrUnknownPrimOp IText IText
  | PsErrTodo
  deriving (Eq, Ord)

instance P.ShowErrorComponent PsError where
  showErrorComponent e =
    case e of
      PsErrUnexpectedToken (L loc tok) ->
        "Unexpected token " ++ show tok ++ " at " ++ show loc
      PsErrBadIndentation -> "bad indent"
      PsErrUnknownPrimOp modname str -> "unknown primop: " ++ pstr
        where pstr = Text.unpack modname.str ++ "." ++ Text.unpack str.str
      PsErrTodo -> "some sort of parse error"