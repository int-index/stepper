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
import Stepper.BuiltIn (BuiltInStrings(..), builtInStrings)

gc :: Module -> TopId -> Maybe Module
gc (Mod bs) entryPoint
  | length bs /= length bs' = Just (Mod bs')
  | otherwise = Nothing
  where bs' = gcTopBindings entryPoint bs

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
            Update b' bs' -> Just (Mod (updateTopBindings b' bs' bs))
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

isValueHNF :: Value TopId -> Bool
isValueHNF v =
  case v of
    RefV{}    -> False
    LitV{}    -> True
    ConAppV{} -> True
    PrimV{}   -> True

isExprWHNF :: ClosedExpr TopId -> Bool
isExprWHNF e =
  case e of
    ValE v  -> isValueHNF v
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
  | Integer_eq <- primop
  = evalstepIntegerEq ctx lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (ValE (PrimV primop) :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (ValE (PrimV primop) :@ lhs :@ rhs')) rhs
evalstepExpr env ctx (CaseE e bs)
  | ValE val <- e, isValueHNF val = evalstepCaseOfVal ctx val bs
  | otherwise = evalstepExpr env (\e' -> ctx (CaseE e' bs)) e
evalstepExpr env ctx (ValE (RefV ref))
  | Just (TopBind _ e) <- Map.lookup ref env
  , isExprWHNF e
  = Update (ctx (extendExprCtx e)) []
  | otherwise = Jump ref
evalstepExpr env ctx (LamE varBndr e1 :@ e2) =
  let (substItem, topBindings) = runState (generateTopBinding env varBndr e2) []
      subst = mkSubst (Const substItem :& HNil)
  in Update (ctx (substExpr subst e1)) topBindings
evalstepExpr env ctx (e1 :@ e2) =
  evalstepExpr env (\e1' -> ctx (e1' :@ e2)) e1
evalstepExpr env ctx (LetE bs e) =
  rightIdListAppend bs $
  let (substItems, topBindings) = runState (generateTopBindings env localBindings) []
      localBindings = hmap (substBinding subst) bs
      subst = mkSubst substItems
  in Update (ctx (substExpr subst e)) topBindings
evalstepExpr _ _ _ = Stuck

evalstepCaseOfVal :: ExprCtx -> Value TopId -> Branches TopId '[] -> Outcome
evalstepCaseOfVal ctx val (Branches bs mb) = go bs
  where
    go [] =
      case mb of
        Nothing -> Stuck
        Just (p :-> e) ->
          case p of
            WildP -> Update (ctx e) []
            VarP{} ->
              let subst = mkSubst (Const val :& HNil)
              in Update (ctx (substExpr subst e)) []
    go ((p :-> e):bs') =
      case p of
        ConAppP con varBndrs
          | ConAppV con' args <- val
          -> if con == con' then
              case hzipWithList (\_ -> Const) varBndrs args of
                  Nothing -> Stuck
                  Just substItems ->
                    rightIdListAppend varBndrs $
                    let subst = mkSubst substItems
                    in Update (ctx (substExpr subst e)) []
            else go bs'
        LitP lit'
          | LitV lit <- val,
            Just eqLit <- matchLit lit lit'
          -> if eqLit
            then Update (ctx e) []
            else go bs'
        _ -> Stuck

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

evalstepIntegerEq ::
  ExprCtx ->
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepIntegerEq ctx lhs rhs
  | ValE (LitV (IntL a)) <- lhs, ValE (LitV (IntL b)) <- rhs
  = Update (ctx (ValE (primBool (a == b)))) []
  | otherwise = Stuck

primBool :: Bool -> Value TopId
primBool True  = ConAppV builtInStrings._True  []
primBool False = ConAppV builtInStrings._False []

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
    goExpr (CaseE e (Branches bs mb)) = do
      goExpr e
      traverse_ goBranch bs
      traverse_ goBranch mb
    goExpr (LetE bs e) = do
      htraverse_ (goExpr . getBindingExpr) bs
      goExpr e

    goBranch :: Branch TopId psort ctx -> State (Set TopId) ()
    goBranch (_ :-> e1) = goExpr e1

    goValueExpr :: ValueExpr TopId ctx -> State (Set TopId) ()
    goValueExpr (RefV ref) = do
      visited <- get
      if Set.member ref visited
      then return ()
      else do
        modify (Set.insert ref)
        traverse_ goExpr (getExpr ref)
    goValueExpr (ConAppV _ args) = traverse_ goValueExpr args
    goValueExpr VarV{} = return ()
    goValueExpr LitV{} = return ()
    goValueExpr PrimV{} = return ()