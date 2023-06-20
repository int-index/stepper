module Stepper.Syntax.Parsed where

import Data.IText

import Stepper.Syntax.Basic

type PCon = IText
type PVar = IText

data PModule = PMod [PBinding]
  deriving (Eq, Show)

data PBinding = PBind PVar PExpr
  deriving (Eq, Show)

data PBranch = PBr PPat PExpr
  deriving (Eq, Show)

data PExpr =
    PVarE PVar
  | PConE PCon
  | PLitE Lit
  | PPrimE PrimOp
  | PLamE PVar PExpr
  | PAppE PExpr PExpr
  | PCaseE PExpr [PBranch]
  | PLetE [PBinding] PExpr         -- recursive let
  deriving (Eq, Show)

data PPat =
    PVarP PVar
  | PConP PCon [PVar]
  | PLitP Lit
  | PWildP
  deriving (Eq, Show)