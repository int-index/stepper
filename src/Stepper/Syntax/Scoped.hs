module Stepper.Syntax.Scoped where

import Data.Kind
import Data.IText
import Data.Inductive

import Stepper.Syntax.Literal

type VarInfo = ()

type VarBndr :: VarInfo -> Type
data VarBndr v where
  VB :: IText -> VarBndr '()

deriving instance Show (VarBndr v)

type Con = IText
type TopId = IText

data Module = Mod [TopBinding]
  deriving Show

data TopBinding = TopBind TopId (ClosedExpr TopId)
  deriving Show

type ClosedExpr :: Type -> Type
type ClosedExpr ref = Expr ref '[]

type Expr :: Type -> [VarInfo] -> Type
data Expr ref ctx where
  RefE :: ref -> Expr ref ctx
  VarE :: Index ctx v -> Expr ref ctx
  ConE :: Con -> Expr ref ctx
  LitE :: Lit -> Expr ref ctx
  LamE :: VarBndr v -> Expr ref (v : ctx) -> Expr ref ctx
  (:@) :: Expr ref ctx -> Expr ref ctx -> Expr ref ctx
  CaseE :: Expr ref ctx -> [Branch ref ctx] -> Expr ref ctx
  LetE ::
    HList (Binding ref (out ++ ctx)) out ->
    Expr ref (out ++ ctx) ->
    Expr ref ctx

infixl 2 :@

deriving instance Show ref => Show (Expr ref ctx)

type Branch :: Type -> [VarInfo] -> Type
data Branch ref ctx where
  (:->) :: Pat out -> Expr ref (out ++ ctx) -> Branch ref ctx

infix 0 :->

deriving instance Show ref => Show (Branch ref ctx)

type Pat :: [VarInfo] -> Type
data Pat out where
  VarP :: VarBndr v -> Pat '[v]
  ConP :: Con -> HList VarBndr out -> Pat out
  LitP :: Lit -> Pat '[]
  WildP :: Pat '[]

deriving instance Show (Pat out)

type Binding :: Type -> [VarInfo] -> VarInfo -> Type
data Binding ref ctx v where
  Bind :: VarBndr v -> Expr ref ctx -> Binding ref ctx v

deriving instance Show ref => Show (Binding ref ctx v)

getBindingVarBndr :: Binding ref ctx v -> VarBndr v
getBindingVarBndr (Bind varBndr _) = varBndr