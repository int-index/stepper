module Stepper.Parser.Context
  ( Parser,
    runParser,
    intern,
    getLayoutColumn,
    popLayoutColumn,
    pushLayoutColumn,
    setLastToken,
    getLastToken,
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import qualified Text.Megaparsec as P
import qualified Control.Monad.State.Strict as S
import Data.IText (IText, ITextPool)
import qualified Data.IText as IText
import Data.Text (Text)

import Stepper.Parser.Error
import Stepper.Parser.Token
import Stepper.BuiltIn

data LayoutContext =
    LayoutTop
  | LayoutColumn Int LayoutContext
  deriving Show

data PState =
  PState {
    pool :: ITextPool,
    layoutContext :: LayoutContext,
    lastToken :: Maybe Token
  }

newtype Parser a = Parser (S.StateT PState (P.Parsec PsError Text) a)
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, P.MonadParsec PsError Text)

setLastToken :: Token -> Parser ()
setLastToken tok = Parser (S.modify updatePState)
  where
    updatePState :: PState -> PState
    updatePState pstate = pstate { lastToken = Just tok }

getLastToken :: Parser (Maybe Token)
getLastToken = Parser (fmap (.lastToken) S.get)

initialPState :: PState
initialPState = PState baseStringPool LayoutTop Nothing

runParser :: Parser a -> FilePath -> Text -> Either String a
runParser (Parser p) path str =
  mapLeft P.errorBundlePretty $
  P.parse (S.evalStateT p initialPState) path str

getLayoutColumn :: Parser Int
getLayoutColumn =
  fmap
    (\pstate -> match pstate.layoutContext)
    (Parser S.get)
  where
    match :: LayoutContext -> Int
    match LayoutTop = P.unPos P.pos1
    match (LayoutColumn n _) = n

popLayoutColumn :: Parser ()
popLayoutColumn = Parser do
  pstate <- S.get
  case pstate.layoutContext of
    LayoutColumn _ parentCtx -> do
      let pstate' = pstate{ layoutContext = parentCtx }
      S.put pstate'
    LayoutTop -> do
      P.customFailure PsErrBadIndentation

pushLayoutColumn :: Int -> Parser ()
pushLayoutColumn n = Parser (S.modify updatePState)
  where
    updatePState :: PState -> PState
    updatePState pstate =
      pstate {
        layoutContext = LayoutColumn n pstate.layoutContext
      }

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

intern :: Text -> Parser IText
intern str = Parser (S.state updatePState)
  where
    updatePState :: PState -> (IText, PState)
    updatePState pstate =
      let
        (istr, pool') = IText.intern str pstate.pool
        pstate' = pstate { pool = pool' }
      in
        (istr, pstate')