{-# LANGUAGE AllowAmbiguousTypes #-}

module Stepper.Syntax.Scoped where

import Data.Kind
import Data.IText
import Data.Inductive
import Data.Functor.Const
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)

import Stepper.Syntax.Basic

type VarInfo = ()

type VarBndr :: VarInfo -> Type
data VarBndr v where
  VB :: !IText -> VarBndr '()

deriving instance Show (VarBndr v)

type Con = IText

data TopId =
    TopIdUser !IText
  | TopIdGen !IText !Int
  deriving (Eq, Ord, Show)

data Phase = Inert | Eval | GarbageMarked

type SPhase :: Phase -> Type
data SPhase phase where
  SInert :: SPhase Inert
  SEval :: SPhase Eval
  SGarbageMarked :: SPhase GarbageMarked

deriving instance Show (SPhase phase)

type family EvalStack phase where
  EvalStack Eval          = NonEmpty TopId
  EvalStack GarbageMarked = NonEmpty TopId
  EvalStack _    = ()

type family LiveSet phase where
  LiveSet GarbageMarked = Set TopId
  LiveSet _             = ()

type Module :: Phase -> Type
data Module phase = Mod [TopBinding] (EvalStack phase) (LiveSet phase)
deriving instance (Show (EvalStack phase), Show (LiveSet phase)) => Show (Module phase)

data TopBinding = TopBind TopId ClosedExpr
  deriving Show

getTopBindingId :: TopBinding -> TopId
getTopBindingId (TopBind name _) = name

getTopBindingExpr :: TopBinding -> ClosedExpr
getTopBindingExpr (TopBind _ e) = e

type Value :: Type
type Value = ValueExpr '[]

-- A value-like expression cannot be reduced,
-- so it is safe to duplicate/inline it.
type ValueExpr :: [VarInfo] -> Type
data ValueExpr ctx where
  RefV :: TopId -> ValueExpr ctx
  VarV :: !(Index ctx v) -> ValueExpr ctx
  LitV :: Lit -> ValueExpr ctx
  ConAppV :: Con -> [ValueExpr ctx] -> ValueExpr ctx

deriving instance Show (ValueExpr ctx)

type ClosedExpr :: Type
type ClosedExpr = Expr '[]

type Expr :: [VarInfo] -> Type
data Expr ctx where
  ValE :: !(ValueExpr ctx) -> Expr ctx
  LamE :: VarBndr v -> Expr (v : ctx) -> Expr ctx
  (:@) :: Expr ctx -> Expr ctx -> Expr ctx
  PrimCallE :: PrimOp -> [Expr ctx] -> Expr ctx
  CaseE :: Expr ctx -> Branches ctx -> Expr ctx
  LetE ::
    HList (Binding (out ++ ctx)) out ->
    Expr (out ++ ctx) ->
    Expr ctx

infixl 2 :@

deriving instance Show (Expr ctx)

type ExprWrapper :: Type
data ExprWrapper where
  CaseEW :: Branches '[] -> ExprWrapper
  ArgEW :: Expr '[] -> ExprWrapper

wrapExpr :: ExprWrapper -> ClosedExpr -> ClosedExpr
wrapExpr (CaseEW bs) e = CaseE e bs
wrapExpr (ArgEW arg) e = e :@ arg

extendExprCtx :: forall ctx' ctx. Expr ctx -> Expr (ctx ++ ctx')
extendExprCtx e0 =
  case e0 of
    ValE val -> ValE (extendValueExprCtx @ctx' val)
    LamE varBndr e -> LamE varBndr (extendExprCtx @ctx' e)
    e1 :@ e2 -> extendExprCtx @ctx' e1 :@ extendExprCtx @ctx' e2
    PrimCallE primop args -> PrimCallE primop (map (extendExprCtx @ctx') args)
    CaseE e bs -> CaseE (extendExprCtx @ctx' e) (extendBranchesCtx @ctx' bs)
    LetE (bs :: HList f out) e ->
      assocListAppend @_ @ctx @ctx' bs $
      LetE
        (hmap (extendBindingCtx @ctx' @ctx @out) bs)
        (extendExprCtx @ctx' e)

extendValueExprCtx :: forall ctx' ctx. ValueExpr ctx -> ValueExpr (ctx ++ ctx')
extendValueExprCtx e0 =
  case e0 of
    RefV ref -> RefV ref
    VarV i -> VarV (extendIndexBase @ctx' i)
    LitV lit -> LitV lit
    ConAppV con args -> ConAppV con (map (extendValueExprCtx @ctx') args)

type Branches :: [VarInfo] -> Type
data Branches ctx =
  Branches
    [Branch MatchPat ctx]
    (Maybe (Branch CatchAllPat ctx))

deriving instance Show (Branches ctx)

type Branch :: PatSort -> [VarInfo] -> Type
data Branch psort ctx where
  (:->) :: Pat psort out -> Expr (out ++ ctx) -> Branch psort ctx

infix 0 :->

deriving instance Show (Branch psort ctx)

extendBranchesCtx :: forall ctx' ctx. Branches ctx -> Branches (ctx ++ ctx')
extendBranchesCtx (Branches bs mb) = Branches bs' mb'
  where
    bs' = map (extendBranchCtx @ctx') bs
    mb' = fmap (extendBranchCtx @ctx') mb

extendBranchCtx :: forall ctx' psort ctx. Branch psort ctx -> Branch psort (ctx ++ ctx')
extendBranchCtx (p :-> e) = p :-> e'
  where
    e' =
      assocListAppend @_ @ctx @ctx' (patVarBndrs p) $
      extendExprCtx @ctx' e

data PatSort = MatchPat | CatchAllPat

type Pat :: PatSort -> [VarInfo] -> Type
data Pat psort out where
  VarP :: VarBndr v -> Pat CatchAllPat '[v]
  WildP :: Pat CatchAllPat '[]
  ConAppP :: Con -> HList VarBndr out -> Pat MatchPat out
  LitP :: Lit -> Pat MatchPat '[]

deriving instance Show (Pat psort out)

patVarBndrs :: Pat psort out -> HList VarBndr out
patVarBndrs p =
  case p of
    VarP varBndr -> varBndr :& HNil
    ConAppP _ varBndrs -> varBndrs
    LitP _ -> HNil
    WildP -> HNil

type Binding :: [VarInfo] -> VarInfo -> Type
data Binding ctx v where
  Bind :: VarBndr v -> Expr ctx -> Binding ctx v

deriving instance Show (Binding ctx v)

extendBindingCtx :: forall ctx' ctx out v. Binding (out ++ ctx) v -> Binding ((out ++ ctx) ++ ctx') v
extendBindingCtx (Bind varBndr e) = Bind varBndr (extendExprCtx @ctx' e)

getBindingVarBndr :: Binding ctx v -> VarBndr v
getBindingVarBndr (Bind varBndr _) = varBndr

getBindingExpr :: Binding ctx v -> Expr ctx
getBindingExpr (Bind _ e) = e

data SubstResult ctx x where
  SubstI :: Index ctx x -> SubstResult ctx x
  SubstE :: (forall ctx'. ValueExpr ctx') -> SubstResult ctx x

newtype Subst ctx ctx' =
  MkSubst (forall x rctx.
    (Index ctx' x -> Index rctx x) ->   -- function to apply to the returned indices (SubstI)
    Index ctx x -> SubstResult rctx x)

applySubst :: Subst ctx ctx' -> Index ctx x -> SubstResult ctx' x
applySubst subst = applySubst' subst id

applySubst' :: Subst ctx ctx' -> forall x rctx. (Index ctx' x -> Index rctx x) -> Index ctx x -> SubstResult rctx x
applySubst' (MkSubst f) = f

mkSubst :: HList (Const Value) ctx -> Subst ctx ctx'
mkSubst HNil =
  MkSubst \_ -> noElements
mkSubst (Const e :& es) =
  MkSubst \cont i ->
    case i of
      Z -> SubstE (extendValueExprCtx e)
      S n -> applySubst' (mkSubst es) cont n

shiftSubstN :: HList f out -> Subst ctx ctx' -> Subst (out ++ ctx) (out ++ ctx')
shiftSubstN HNil = id
shiftSubstN (_ :& xs) = shiftSubst1 . shiftSubstN xs

shiftSubst1 :: Subst ctx ctx' -> Subst (x : ctx) (x : ctx')
shiftSubst1 subst =
  MkSubst \cont i ->
    case i of
      Z -> SubstI (cont Z)
      S n -> applySubst' subst (cont . S) n

substExpr :: Subst ctx ctx' -> Expr ctx -> Expr ctx'
substExpr subst e0 =
  case e0 of
    ValE val -> ValE (substValueExpr subst val)
    LamE varBndr e ->
      let subst' = shiftSubst1 subst
      in LamE varBndr (substExpr subst' e)
    e1 :@ e2 -> substExpr subst e1 :@ substExpr subst e2
    PrimCallE primop args -> PrimCallE primop (map (substExpr subst) args)
    CaseE e bs -> CaseE (substExpr subst e) (substBranches subst bs)
    LetE bs e ->
      let subst' = shiftSubstN bs subst
      in LetE (hmap (substBinding subst') bs) (substExpr subst' e)

substValueExpr :: Subst ctx ctx' -> ValueExpr ctx -> ValueExpr ctx'
substValueExpr subst e0 =
  case e0 of
    RefV ref -> RefV ref
    VarV i ->
      case applySubst subst i of
        SubstI j -> VarV j
        SubstE e -> e
    LitV lit -> LitV lit
    ConAppV con args -> ConAppV con (map (substValueExpr subst) args)

substBranches :: Subst ctx ctx' -> Branches ctx -> Branches ctx'
substBranches subst (Branches bs mb) = Branches (map (substBranch subst) bs) (fmap (substBranch subst) mb)

substBranch :: Subst ctx ctx' -> Branch psort ctx -> Branch psort ctx'
substBranch subst (p :-> e) =
  let subst' = shiftSubstN (patVarBndrs p) subst
  in p :-> substExpr subst' e

substBinding :: Subst ctx ctx' -> Binding ctx x -> Binding ctx' x
substBinding subst (Bind varBndr e) = Bind varBndr (substExpr subst e)