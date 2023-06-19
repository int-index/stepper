{
{-# LANGUAGE NoStrictData #-}
module Stepper.Parser
  ( parseExpr,
    parseModule,
  ) where

import Stepper.Located
import Stepper.Syntax.Literal
import Stepper.Syntax.Parsed
import Stepper.Parser.Lexer
import Stepper.Parser.Token
import Stepper.Parser.Context
import Stepper.Parser.Error

import Data.Void
import qualified Text.Megaparsec as P
import Data.Text (Text)
import Data.IText (IText)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as List.NonEmpty
}

%expect 0

%name pExpr Expr
%name pModule Module

%tokentype { Located Token }
%error { parseError }
%monad { Parser } { >>= } { return }
%lexer { (parseToken >>=) } { L _ TokenEOF }

%token
  '='  { L _ TokenEq }
  ';'  { L _ TokenSemicolon }
  '('  { L _ TokenLPar }
  ')'  { L _ TokenRPar }
  '{'  { L _ TokenLCuBr }
  '}'  { L _ TokenRCuBr }
  '\\' { L _ TokenBackslash }
  '->' { L _ TokenArrRight }
  'let' { L _ TokenKwLet }
  'in' { L _ TokenKwIn }
  'case' { L _ TokenKwCase }
  'of' { L _ TokenKwOf }
  '_'  { L _ TokenUnderscore }
  nat { L _ (TokenNatLit _) }
  int { L _ (TokenIntLit _) }
  frc { L _ (TokenFrcLit _) }
  str { L _ (TokenStrLit _) }
  chr { L _ (TokenChrLit _) }
  var { L _ (TokenIdent VariableName _) }
  con { L _ (TokenIdent ConstrName _) }
  varop { L _ (TokenOpIdent VariableName _) }
  conop { L _ (TokenOpIdent ConstrName _) }
  start_layout { L _ TokenStartLayout }
  end_layout { L _ TokenEndLayout }

%%

Semis(b) :    -- reversed
    Semis(b) ';' b { $3 : $1 }
  | Semis(b) ';'   { $1 }
  | b              { [$1] }
  |                { [] }

Block(b) :    -- reversed
    '{' Semis(b) '}' { $2 }
  | start_layout Semis(b) EndLayout { $2 }

EndLayout :: { () }
EndLayout :
    end_layout { () }
  | error      {% popLayoutColumn }

Module :: { PModule }
Module :
  Semis(Binding) { PMod (reverse $1) }

Binding :: { PBinding }
Binding :
  Var '=' Expr { PBind $1 $3 }

Expr :: { PExpr }
Expr :
    AtomExpr { $1 }
  | Expr AtomExpr { PAppE $1 $2 }

Var :: { IText }
Var :
    var           { getIdent $1 }
  | '(' varop ')' { getOpIdent $2 }

Con :: { IText }
Con :
    con           { getIdent $1 }
  | '(' conop ')' { getOpIdent $2}

Vars :: { [IText] }   -- reversed
Vars :
    Vars Var    { $2 : $1 }
  |             { [] }

AtomExpr :: { PExpr }
AtomExpr :
    Var { PVarE $1 }
  | Con { PConE $1 }
  | Lit { PLitE $1 }
  | '\\' Vars '->' Expr %shift { foldl (flip PLamE) $4 $2 }
  | 'case' Expr 'of' Block(Branch) %shift { PCaseE $2 (reverse $4) }
  | 'let' Block(Binding) 'in' Expr %shift { PLetE (reverse $2) $4 }
  | '(' Expr ')' { $2 }

Branch :: { PBranch }
Branch : Pat '->' Expr { PBr $1 $3 }

Pat :: { PPat }
Pat :
    Var      { PVarP $1 }
  | Con Vars { PConP $1 (reverse $2) }
  | Lit      { PLitP $1 }
  | '_'      { PWildP }

Lit :: { Lit }
Lit :
    nat   { NatL (getNatLit $1) }
  | int   { IntL (getIntLit $1) }
  | frc   { FrcL (getFrcLit $1) }
  | str   { StrL (getStrLit $1) }
  | chr   { ChrL (getChrLit $1) }

{
parseExpr :: FilePath -> Text -> Either String PExpr
parseExpr = runParser pExpr

parseModule :: FilePath -> Text -> Either String PModule
parseModule = runParser pModule

parseError :: Located Token -> Parser a
parseError ltok = P.customFailure (PsErrUnexpectedToken ltok)

getNatLit (L _ (TokenNatLit a)) = a
getIntLit (L _ (TokenIntLit a)) = a
getFrcLit (L _ (TokenFrcLit a)) = a
getStrLit (L _ (TokenStrLit a)) = a
getChrLit (L _ (TokenChrLit a)) = a
getIdent (L _ (TokenIdent _ a)) = a
getOpIdent (L _ (TokenOpIdent _ a)) = a

}