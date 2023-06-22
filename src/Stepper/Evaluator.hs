module Stepper.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Const
import Data.Inductive
import Numeric.Natural
import Control.Monad.State

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped

evalstep :: Module -> TopId -> Maybe Module  -- Nothing <=> nothing to reduce
evalstep (Mod bs) = go
  where
    env = Map.fromList [ (name, b) | b@(TopBind name _) <- bs ]
    go name = do
      b <- Map.lookup name env
      case evalstepTopBinding env b of
        Stuck -> Nothing
        Update b' bs' -> Just (Mod (updateTopBindings b' bs' bs))
        Jump name' -> go name'

updateTopBindings :: TopBinding -> [TopBinding] -> [TopBinding] -> [TopBinding]
updateTopBindings _ _ [] = []
updateTopBindings b@(TopBind name _) newBindings (b'@(TopBind name' _) : bs)
  | name == name' = b : newBindings ++ bs
  | otherwise = b' : updateTopBindings b newBindings bs

data Outcome =
    Stuck
  | Update TopBinding [TopBinding]
  | Jump TopId

orIfStuck :: Outcome -> Outcome -> Outcome
Stuck `orIfStuck` r = r
r `orIfStuck` _ = r

evalstepTopBinding :: TopEnv -> TopBinding -> Outcome
evalstepTopBinding env (TopBind name e) = evalstepExpr env (TopBind name) e

type ExprCtx = ClosedExpr TopId -> TopBinding

type TopEnv = Map TopId TopBinding

isWHNF :: Expr TopId ctx -> Bool
isWHNF e =
  case e of
    RefE{}  -> False
    VarE{}  -> True
    ConE{}  -> True
    LitE{}  -> True
    PrimE{} -> True
    LamE{}  -> True
    _ :@ _  -> False
    CaseE{} -> False
    LetE{}  -> False

isSmall :: Expr ref ctx -> Bool
isSmall e =
  case e of
    RefE{} -> True
    VarE{} -> True
    ConE{} -> True
    LitE{} -> True
    PrimE{} -> True
    LamE{} -> False
    _ :@ _ -> False
    CaseE{} -> False
    LetE{} -> False

freshId :: TopEnv -> VarBndr v -> TopId
freshId env (VB x) =
  case [n | (TopIdGen x' n, _) <- Map.toDescList env, x == x'] of
    []  -> TopIdGen x 0
    n:_ -> TopIdGen x (n + 1)

-- NB: We assume that the VarBndrs in the list have unique names.
-- generateTopBindings ::
--   TopEnv ->
--   HList VarBndr out ->
--   HList (Const (ClosedExpr TopId)) out ->
--   State [TopBinding] (HList (Const (ClosedExpr TopId)) out)
-- generateTopBindings _ HNil HNil = return HNil
-- generateTopBindings env (varBndr :& varBndrs) (Const e :& es) = do
--   e' <- generateTopBinding env varBndr e
--   fmap (Const e' :&) (generateTopBindings env varBndrs es)

generateTopBindings :: TopEnv -> HList (Binding TopId '[]) out -> State [TopBinding] (HList (Const (ClosedExpr TopId)) out)
generateTopBindings _ HNil = return HNil
generateTopBindings env (Bind varBndr e :& bs) = do
  e' <- generateTopBinding env varBndr e
  fmap (Const e' :&) (generateTopBindings env bs)

generateTopBinding :: TopEnv -> VarBndr v -> Expr TopId '[] -> State [TopBinding] (Expr TopId '[])
generateTopBinding env varBndr e
  | isSmall e = return e
  | otherwise = do
      let x = freshId env varBndr
      modify (TopBind x e :)
      return (RefE x)

evalstepExpr :: TopEnv -> ExprCtx -> ClosedExpr TopId -> Outcome
evalstepExpr env ctx (PrimE primop :@ lhs :@ rhs)
  | Just f <- matchNaturalBinOp primop
  = evalstepNaturalBinOp ctx f lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (PrimE primop :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (PrimE primop :@ lhs :@ rhs')) rhs
  | Just f <- matchIntegerBinOp primop
  = evalstepIntegerBinOp ctx f lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (PrimE primop :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (PrimE primop :@ lhs :@ rhs')) rhs
evalstepExpr env ctx (CaseE (LitE lit) bs)
  = evalstepCaseOfLit env ctx lit bs
evalstepExpr env ctx (RefE ref)
  | Just (TopBind _ e) <- Map.lookup ref env
  , isWHNF e
  = Update (ctx (extendExprCtx e)) []
  | otherwise = Jump ref
evalstepExpr env ctx (LamE varBndr e1 :@ e2)
  | isSmall e2 =
    let subst = mkSubst (Const e2 :& HNil)
    in Update (ctx (substExpr subst e1)) []
  | otherwise =
    let x = freshId env varBndr
        subst = mkSubst (Const (RefE x) :& HNil)
    in Update (ctx (substExpr subst e1)) [TopBind x e2]
evalstepExpr env ctx (e1 :@ e2) =
  evalstepExpr env (\e1' -> ctx (e1' :@ e2)) e1
evalstepExpr env ctx (CaseE e bs) = evalstepExpr env (\e' -> ctx (CaseE e' bs)) e
evalstepExpr env ctx (LetE (bs :: HList f out) e) =
  rightIdListAppend bs $
  let substItems :: HList (Const (ClosedExpr TopId)) out
      genTopBindings :: State [TopBinding] (HList (Const (ClosedExpr TopId)) out)
      topBindings :: [TopBinding]
      subst :: Subst TopId out '[]
      (substItems, topBindings) = runState genTopBindings []
      genTopBindings = generateTopBindings env (hmap (substBinding subst) bs)
      subst = mkSubst substItems
  in Update (ctx (substExpr subst e)) topBindings
evalstepExpr _ _ _ = Stuck

evalstepCaseOfLit :: TopEnv -> ExprCtx -> Lit -> [Branch TopId '[]] -> Outcome
evalstepCaseOfLit _ _ _ [] = Stuck
evalstepCaseOfLit env ctx lit ((p :-> e):bs) =
  case p of
    VarP{} ->
      let subst = mkSubst (Const (LitE lit) :& HNil)
      in Update (ctx (substExpr subst e)) []
    ConP{} -> Stuck
    LitP lit' ->
      case matchLit lit lit' of
        Nothing    -> Stuck
        Just True  -> Update (ctx e) []
        Just False -> evalstepCaseOfLit env ctx lit bs
    WildP  -> Update (ctx e) []

matchLit :: Lit -> Lit -> Maybe Bool
matchLit (NatL a) (NatL b) = Just (a == b)
matchLit (IntL a) (IntL b) = Just (a == b)
matchLit (FrcL a) (FrcL b) = Just (a == b)
matchLit (StrL a) (StrL b) = Just (a == b)
matchLit (ChrL a) (ChrL b) = Just (a == b)
matchLit _ _ = Nothing

evalstepNaturalBinOp ::
  ExprCtx ->
  (Natural -> Natural -> Natural) ->
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepNaturalBinOp ctx f lhs rhs
  | LitE (NatL a) <- lhs, LitE (NatL b) <- rhs
  = Update (ctx (LitE (NatL (f a b)))) []
  | otherwise = Stuck

evalstepIntegerBinOp ::
  ExprCtx ->
  (Integer -> Integer -> Integer) ->
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepIntegerBinOp ctx f lhs rhs
  | LitE (IntL a) <- lhs, LitE (IntL b) <- rhs
  = Update (ctx (LitE (IntL (f a b)))) []
  | otherwise = Stuck

matchNaturalBinOp :: PrimOp -> Maybe (Natural -> Natural -> Natural)
matchNaturalBinOp primop =
  case primop of
    Natural_add -> Just (+)
    Natural_sub -> Just (-)
    Natural_mul -> Just (*)
    Natural_div -> Just div
    _ -> Nothing

matchIntegerBinOp :: PrimOp -> Maybe (Integer -> Integer -> Integer)
matchIntegerBinOp primop =
  case primop of
    Integer_add -> Just (+)
    Integer_sub -> Just (-)
    Integer_mul -> Just (*)
    Integer_div -> Just div
    _ -> Nothing