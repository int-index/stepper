module Stepper.Parser.Lexer
  ( parseToken
  ) where

import Control.Monad
import Data.Foldable
import Numeric.Natural (Natural)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P.L
import Data.Maybe
import Data.Char
import Data.IText (IText)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Stepper.Parser.Token
import Stepper.Parser.Context
import Stepper.BuiltIn
import Stepper.Located

settingLastToken :: (a -> Token) -> Parser a -> Parser a
settingLastToken f m = do
  r <- m
  setLastToken (f r)
  return r

parseToken :: Parser (Located Token)
parseToken = settingLastToken (.located) do
  is_nl <- pSpaces
  is_eof <- P.atEnd
  pos <- P.getSourcePos     -- FIXME: this is slow (linear pass from the start of the string)
  if is_eof then do
    let loc = mkSourceSpan pos pos
    return (L loc TokenEOF)
  else
    parseLayoutToken is_nl pos P.<|>
    parseLocatedToken pos pToken

parseLocatedToken ::
  SourcePos ->
  Parser Token ->
  Parser (Located Token)
parseLocatedToken pos pTok = do
  tok <- pTok
  pos' <- P.getSourcePos     -- FIXME: this is slow (linear pass from the start of the string)
  let loc = mkSourceSpan pos pos'
  return (L loc tok)

parseLayoutToken ::
  Bool ->
  SourcePos ->
  Parser (Located Token)
parseLayoutToken is_nl pos = do
  layout_col <- getLayoutColumn
  m_last_tok <- getLastToken
  let layout_herald = maybe False isLayoutHerald m_last_tok
  if
    | is_nl, col == layout_col -> emit_semi
    | col < layout_col -> end_layout
    | layout_herald -> curly_brace P.<|> start_layout
    | otherwise -> P.empty
  where
    col = unPos pos.sourceColumn
    loc = mkSourceSpan pos pos
    emit_semi = return (L loc TokenSemicolon)
    end_layout = do
      popLayoutColumn
      return (L loc TokenEndLayout)
    start_layout = do
      pushLayoutColumn (unPos pos.sourceColumn)
      return (L loc TokenStartLayout)
    curly_brace = parseLocatedToken pos pLCuBr

-- Returns True if a newline was encountered.
pSpaces :: Parser Bool
pSpaces = do
  -- Skip whitespace on the current line
  _ <- P.takeWhileP label is_spc
  -- Skip the newline, if any
  nl <- fmap isJust (P.optional (P.char '\n'))
  when nl do
    -- Skip whitespace on subsequent lines
    void $ P.takeWhileP label (\c -> is_spc c || is_nl c)
  return nl
  where
    label = Just "whitespace"

    is_spc ' ' = True
    is_spc _ = False

    is_nl '\n' = True
    is_nl _ = False

pLCuBr :: Parser Token
pLCuBr = TokenLCuBr <$ P.char '{'

isLayoutHerald :: Token -> Bool
isLayoutHerald tok =
  case tok of
    TokenKwLet   -> True
    TokenKwOf    -> True
    _            -> False

pToken :: Parser Token
pToken =
  asum [
    TokenLPar  <$ P.char '(',
    TokenRPar  <$ P.char ')',
    TokenLSqBr <$ P.char '[',
    TokenRSqBr <$ P.char ']',
    TokenLCuBr <$ P.char '{',
    TokenRCuBr <$ P.char '}',
    TokenComma <$ P.char ',',
    TokenSemicolon <$ P.char ';',
    pNumber,
    mkTokenIdent <$> pIdent,
    pPunct,
    TokenStrLit <$> pStrLit,
    TokenChrLit <$> pChrLit
  ]


data NumericSign = NumericPlus | NumericMinus

-- Natural: 0, 1, 2, 953
-- Integer: -42, -3, -1, +0, +3, +71
-- Fractional: -123.0, -123.5, 0.18, 0.5, +0.5, +99.1
pNumber :: Parser Token
pNumber = P.try do
  msign <- P.optional $
    asum [
      NumericMinus <$ P.char '-',
      NumericPlus  <$ P.char '+'
    ]
  digits <- P.some pDigit
  mfracdigits <- P.optional do
    void $ P.char '.'
    P.some pDigit
  return $
    case (msign, mfracdigits) of
      (Nothing, Nothing)   -> TokenNatLit (mkNatural digits)
      (Just sgn, Nothing)  -> TokenIntLit (mkInteger sgn digits)
      (_, Just fracdigits) -> TokenFrcLit (mkFractional msign digits fracdigits)

mkNatural :: [Int] -> Natural
mkNatural = foldDecimalDigits

mkInteger :: NumericSign -> [Int] -> Integer
mkInteger sgn digits = applyNumericSign sgn (foldDecimalDigits digits)

mkFractional :: Maybe NumericSign -> [Int] -> [Int] -> Rational
mkFractional msign digits fracdigits =
  applyNumericSign (fromMaybe NumericPlus msign) $
  (/ (10 ^ length fracdigits)) $
  foldDecimalDigits (digits ++ fracdigits)

foldDecimalDigits :: Num a => [Int] -> a
foldDecimalDigits = foldl' (\acc n -> acc * 10 + fromIntegral n) 0

applyNumericSign :: Num a => NumericSign -> a -> a
applyNumericSign NumericPlus  = id
applyNumericSign NumericMinus = negate

pDigit :: Parser Int
pDigit = fmap digitToInt (P.satisfy isDigit)

pIdent :: Parser IText
pIdent = do
  str <- P.takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_' || c == '\'')
  intern str

isQuote :: Char -> Bool
isQuote '\'' = True
isQuote '\"' = True
isQuote _ = False

isOpenClosePunctuation :: Char -> Bool
isOpenClosePunctuation c =
  case generalCategory c of
    OpenPunctuation -> True
    ClosePunctuation -> True
    _ -> False

pPunct :: Parser Token
pPunct = do
  let isPunct c =
        not (isQuote c || isOpenClosePunctuation c) &&
        (isPunctuation c || isSymbol c)
  str <- P.takeWhile1P (Just "punctuation") isPunct
  case str of
    "=" -> pure TokenEq
    "->" -> pure TokenArrRight
    _ -> fmap mkTokenOpIdent (intern str)

keywords :: HashMap IText Token
keywords =
  HashMap.fromList
    [
      (builtInStrings._let, TokenKwLet),
      (builtInStrings._in, TokenKwIn),
      (builtInStrings._case, TokenKwCase),
      (builtInStrings._of, TokenKwOf),
      (builtInStrings.__underscore, TokenUnderscore)
    ]

opkeywords :: HashMap IText Token
opkeywords =
  HashMap.fromList
    [
      (builtInStrings.__backslash, TokenBackslash)
    ]

mkTokenIdent :: IText -> Token
mkTokenIdent istr
  | Just kw <- HashMap.lookup istr keywords
  = kw
mkTokenIdent istr = TokenIdent constr_or_var istr
  where
    constr_or_var =
      case Text.uncons istr.str of
        Just (c, _) | isUpper c -> ConstrName
        _ -> VariableName

mkTokenOpIdent :: IText -> Token
mkTokenOpIdent istr
  | Just kw <- HashMap.lookup istr opkeywords
  = kw
mkTokenOpIdent istr = TokenOpIdent constr_or_var istr
  where
    constr_or_var =
      case Text.uncons istr.str of
        Just (':', _) -> ConstrName
        _ -> VariableName

pStrLit :: Parser Text
pStrLit = fmap Text.pack (P.char '"' *> P.manyTill P.L.charLiteral (P.char '"'))

pChrLit :: Parser Char
pChrLit = P.between (P.char '\'') (P.char '\'') P.L.charLiteral