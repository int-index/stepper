module Stepper.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Const
import Data.Inductive
import Numeric.Natural
import Control.Monad.State
import Data.Foldable

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped

evalstep :: Module -> TopId -> Maybe Module  -- Nothing <=> nothing to reduce
evalstep (Mod bs) entryPoint = go Set.empty entryPoint
  where
    env = mkTopEnv bs
    go visited name
      | Set.member name visited = Nothing
      | otherwise = do
          b <- Map.lookup name env
          case evalstepTopBinding env b of
            Stuck -> Nothing
            Update b' bs' -> Just (Mod (gcTopBindings entryPoint (updateTopBindings b' bs' bs)))
            Jump name' -> go (Set.insert name visited) name'


mkTopEnv :: [TopBinding] -> Map TopId TopBinding
mkTopEnv bs = Map.fromList [ (name, b) | b@(TopBind name _) <- bs ]

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
    ValE RefV{}  -> False
    ValE VarV{}  -> True
    ValE LitV{}  -> True
    ValE ConV{}  -> True
    ValE PrimV{} -> True
    LamE{}  -> True
    _ :@ _  -> False
    CaseE{} -> False
    LetE{}  -> False

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

generateTopBindings :: TopEnv -> HList (Binding TopId '[]) out -> State [TopBinding] (HList (Const (Value TopId)) out)
generateTopBindings _ HNil = return HNil
generateTopBindings env (Bind varBndr e :& bs) = do
  e' <- generateTopBinding env varBndr e
  fmap (Const e' :&) (generateTopBindings env bs)

generateTopBinding :: TopEnv -> VarBndr v -> Expr TopId '[] -> State [TopBinding] (Value TopId)
generateTopBinding _ _ (ValE val) = return val
generateTopBinding env varBndr e = do
  let x = freshId env varBndr
  modify (TopBind x e :)
  return (RefV x)

evalstepExpr :: TopEnv -> ExprCtx -> ClosedExpr TopId -> Outcome
evalstepExpr env ctx (ValE (PrimV primop) :@ lhs :@ rhs)
  | Just f <- matchNaturalBinOp primop
  = evalstepNaturalBinOp ctx f lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (ValE (PrimV primop) :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (ValE (PrimV primop) :@ lhs :@ rhs')) rhs
  | Just f <- matchIntegerBinOp primop
  = evalstepIntegerBinOp ctx f lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (ValE (PrimV primop) :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (ValE (PrimV primop) :@ lhs :@ rhs')) rhs
evalstepExpr env ctx (CaseE (ValE (LitV lit)) bs)
  = evalstepCaseOfLit env ctx lit bs
evalstepExpr env ctx (ValE (RefV ref))
  | Just (TopBind _ e) <- Map.lookup ref env
  , isWHNF e
  = Update (ctx (extendExprCtx e)) []
  | otherwise = Jump ref
evalstepExpr env ctx (LamE varBndr e1 :@ e2) =
  let (substItem, topBindings) = runState (generateTopBinding env varBndr e2) []
      subst = mkSubst (Const substItem :& HNil)
  in Update (ctx (substExpr subst e1)) topBindings
evalstepExpr env ctx (e1 :@ e2) =
  evalstepExpr env (\e1' -> ctx (e1' :@ e2)) e1
evalstepExpr env ctx (CaseE e bs) = evalstepExpr env (\e' -> ctx (CaseE e' bs)) e
evalstepExpr env ctx (LetE bs e) =
  rightIdListAppend bs $
  let (substItems, topBindings) = runState (generateTopBindings env localBindings) []
      localBindings = hmap (substBinding subst) bs
      subst = mkSubst substItems
  in Update (ctx (substExpr subst e)) topBindings
evalstepExpr _ _ _ = Stuck

evalstepCaseOfLit :: TopEnv -> ExprCtx -> Lit -> [Branch TopId '[]] -> Outcome
evalstepCaseOfLit _ _ _ [] = Stuck
evalstepCaseOfLit env ctx lit ((p :-> e):bs) =
  case p of
    VarP{} ->
      let subst = mkSubst (Const (LitV lit) :& HNil)
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
  | ValE (LitV (NatL a)) <- lhs, ValE (LitV (NatL b)) <- rhs
  = Update (ctx (ValE (LitV (NatL (f a b))))) []
  | otherwise = Stuck

evalstepIntegerBinOp ::
  ExprCtx ->
  (Integer -> Integer -> Integer) ->
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepIntegerBinOp ctx f lhs rhs
  | ValE (LitV (IntL a)) <- lhs, ValE (LitV (IntL b)) <- rhs
  = Update (ctx (ValE (LitV (IntL (f a b))))) []
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

gcTopBindings :: TopId -> [TopBinding] -> [TopBinding]
gcTopBindings root topBinds = filter isLive topBinds
  where
    env = mkTopEnv topBinds
    liveSet =
      case getExpr root of
        Nothing -> Set.empty
        Just e -> execState (goExpr e) (Set.singleton root)

    isLive :: TopBinding -> Bool
    isLive b = Set.member (getTopBindingId b) liveSet

    getExpr :: TopId -> Maybe (ClosedExpr TopId)
    getExpr name = fmap getTopBindingExpr (Map.lookup name env)

    goExpr :: Expr TopId ctx -> State (Set TopId) ()
    goExpr (ValE e) = goValueExpr e
    goExpr (e1 :@ e2) = do
      goExpr e1
      goExpr e2
    goExpr (LamE _ e) = goExpr e
    goExpr (CaseE e bs) = do
      goExpr e
      traverse_ (\(_ :-> e1) -> goExpr e1) bs
    goExpr (LetE bs e) = do
      htraverse_ (goExpr . getBindingExpr) bs
      goExpr e

    goValueExpr :: ValueExpr TopId ctx -> State (Set TopId) ()
    goValueExpr (RefV ref) = do
      visited <- get
      if Set.member ref visited
      then return ()
      else do
        modify (Set.insert ref)
        traverse_ goExpr (getExpr ref)
    goValueExpr VarV{} = return ()
    goValueExpr ConV{} = return ()
    goValueExpr LitV{} = return ()
    goValueExpr PrimV{} = return ()