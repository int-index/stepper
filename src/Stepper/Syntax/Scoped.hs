{-# LANGUAGE AllowAmbiguousTypes #-}

module Stepper.Syntax.Scoped where

import Data.Kind
import Data.IText
import Data.Inductive
import Data.Functor.Const

import Stepper.Syntax.Basic

type VarInfo = ()

type VarBndr :: VarInfo -> Type
data VarBndr v where
  VB :: IText -> VarBndr '()

deriving instance Show (VarBndr v)

type Con = IText

data TopId =
    TopIdUser IText
  | TopIdGen IText Int
  deriving (Eq, Ord, Show)

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
  PrimE :: PrimOp -> Expr ref ctx
  LamE :: VarBndr v -> Expr ref (v : ctx) -> Expr ref ctx
  (:@) :: Expr ref ctx -> Expr ref ctx -> Expr ref ctx
  CaseE :: Expr ref ctx -> [Branch ref ctx] -> Expr ref ctx
  LetE ::
    HList (Binding ref (out ++ ctx)) out ->
    Expr ref (out ++ ctx) ->
    Expr ref ctx

infixl 2 :@

deriving instance Show ref => Show (Expr ref ctx)

extendExprCtx :: forall ctx' ctx ref. Expr ref ctx -> Expr ref (ctx ++ ctx')
extendExprCtx e0 =
  case e0 of
    RefE ref -> RefE ref
    VarE i -> VarE (extendIndexBase @ctx' i)
    ConE con -> ConE con
    LitE lit -> LitE lit
    PrimE primop -> PrimE primop
    LamE varBndr e -> LamE varBndr (extendExprCtx @ctx' e)
    e1 :@ e2 -> extendExprCtx @ctx' e1 :@ extendExprCtx @ctx' e2
    CaseE e bs -> CaseE (extendExprCtx @ctx' e) (map (extendBranchCtx @ctx') bs)
    LetE (bs :: HList f out) e ->
      assocListAppend @_ @ctx @ctx' bs $
      LetE
        (hmap (extendBindingCtx @ctx' @ctx @out) bs)
        (extendExprCtx @ctx' e)

type Branch :: Type -> [VarInfo] -> Type
data Branch ref ctx where
  (:->) :: Pat out -> Expr ref (out ++ ctx) -> Branch ref ctx

infix 0 :->

deriving instance Show ref => Show (Branch ref ctx)

extendBranchCtx :: forall ctx' ref ctx. Branch ref ctx -> Branch ref (ctx ++ ctx')
extendBranchCtx (p :-> e) = p :-> e'
  where
    e' =
      assocListAppend @_ @ctx @ctx' (patVarBndrs p) $
      extendExprCtx @ctx' e

type Pat :: [VarInfo] -> Type
data Pat out where
  VarP :: VarBndr v -> Pat '[v]
  ConP :: Con -> HList VarBndr out -> Pat out
  LitP :: Lit -> Pat '[]
  WildP :: Pat '[]

deriving instance Show (Pat out)

patVarBndrs :: Pat out -> HList VarBndr out
patVarBndrs p =
  case p of
    VarP varBndr -> varBndr :& HNil
    ConP _ varBndrs -> varBndrs
    LitP _ -> HNil
    WildP -> HNil

type Binding :: Type -> [VarInfo] -> VarInfo -> Type
data Binding ref ctx v where
  Bind :: VarBndr v -> Expr ref ctx -> Binding ref ctx v

deriving instance Show ref => Show (Binding ref ctx v)

extendBindingCtx :: forall ctx' ctx out ref v. Binding ref (out ++ ctx) v -> Binding ref ((out ++ ctx) ++ ctx') v
extendBindingCtx (Bind varBndr e) = Bind varBndr (extendExprCtx @ctx' e)

getBindingVarBndr :: Binding ref ctx v -> VarBndr v
getBindingVarBndr (Bind varBndr _) = varBndr

data SubstResult ref ctx x where
  SubstI :: Index ctx x -> SubstResult ref ctx x
  SubstE :: (forall ctx'. Expr ref ctx') -> SubstResult ref ctx x

newtype Subst ref ctx ctx' =
  MkSubst (forall x rctx.
    (Index ctx' x -> Index rctx x) ->   -- function to apply to the returned indices (SubstI)
    Index ctx x -> SubstResult ref rctx x)

applySubst :: Subst ref ctx ctx' -> Index ctx x -> SubstResult ref ctx' x
applySubst subst = applySubst' subst id

applySubst' :: Subst ref ctx ctx' -> forall x rctx. (Index ctx' x -> Index rctx x) -> Index ctx x -> SubstResult ref rctx x
applySubst' (MkSubst f) = f

mkSubst :: HList (Const (ClosedExpr ref)) ctx -> Subst ref ctx ctx'
mkSubst HNil =
  MkSubst \_ -> noElements
mkSubst (Const e :& es) =
  MkSubst \cont i ->
    case i of
      Z -> SubstE (extendExprCtx e)
      S n -> applySubst' (mkSubst es) cont n

shiftSubstN :: HList f out -> Subst ref ctx ctx' -> Subst ref (out ++ ctx) (out ++ ctx')
shiftSubstN HNil = id
shiftSubstN (_ :& xs) = shiftSubst1 . shiftSubstN xs

shiftSubst1 :: Subst ref ctx ctx' -> Subst ref (x : ctx) (x : ctx')
shiftSubst1 subst =
  MkSubst \cont i ->
    case i of
      Z -> SubstI (cont Z)
      S n -> applySubst' subst (cont . S) n

substExpr :: Subst ref ctx ctx' -> Expr ref ctx -> Expr ref ctx'
substExpr subst e0 =
  case e0 of
    RefE ref -> RefE ref
    VarE i ->
      case applySubst subst i of
        SubstI j -> VarE j
        SubstE e -> e
    ConE con -> ConE con
    LitE lit -> LitE lit
    PrimE primop -> PrimE primop
    LamE varBndr e ->
      let subst' = shiftSubst1 subst
      in LamE varBndr (substExpr subst' e)
    e1 :@ e2 -> substExpr subst e1 :@ substExpr subst e2
    CaseE e bs -> CaseE (substExpr subst e) (map (substBranch subst) bs)
    LetE bs e ->
      let subst' = shiftSubstN bs subst
      in LetE (hmap (substBinding subst') bs) (substExpr subst' e)

substBranch :: Subst ref ctx ctx' -> Branch ref ctx -> Branch ref ctx'
substBranch subst (p :-> e) =
  let subst' = shiftSubstN (patVarBndrs p) subst
  in p :-> substExpr subst' e

substBinding :: Subst ref ctx ctx' -> Binding ref ctx x -> Binding ref ctx' x
substBinding subst (Bind varBndr e) = Bind varBndr (substExpr subst e)