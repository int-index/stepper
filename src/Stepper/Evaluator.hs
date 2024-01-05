module Stepper.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Const
import Data.Inductive
import Numeric.Natural
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Maybe

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped
import Stepper.BuiltIn (BuiltInStrings(..), builtInStrings)

gcMark :: Module Inert -> TopId -> Maybe (Module GarbageMarked)
gcMark (Mod bs) entryPoint =
  do
    let bs' = gcMarkTopBindings entryPoint bs
    guard (any isDead bs')
    Just (Mod bs')
  where
    isDead (TopBind Dead _ _) = True
    isDead _ = False

gcSweep :: Module GarbageMarked -> Module Inert
gcSweep (Mod bs) = Mod (gcSweepTopBindings bs)

evalstep :: Module Inert -> TopId -> Maybe (Module Inert)  -- Nothing <=> nothing to reduce
evalstep (Mod bs) entryPoint = go Set.empty entryPoint
  where
    env = mkTopEnv bs
    go visited name
      | Set.member name visited = Nothing
      | otherwise = do
          TopBind _ _ e <- Map.lookup name env
          case evalstepExpr env e of
            Stuck -> Nothing
            Update e' bs' -> Just (Mod (updateTopBinding name e' bs ++ bs'))
            Jump name' -> go (Set.insert name visited) name'

mkTopEnv :: [TopBinding Inert] -> Map TopId (TopBinding Inert)
mkTopEnv bs = Map.fromList [ (name, b) | b@(TopBind _ name _) <- bs ]

updateTopBinding :: TopId -> ClosedExpr TopId -> [TopBinding Inert] -> [TopBinding Inert]
updateTopBinding _ _ [] = []
updateTopBinding name e (b'@(TopBind _ name' _) : bs)
  | name == name' = TopBind () name e : bs
  | otherwise = b' : updateTopBinding name e bs

data Outcome =
    Stuck
  | Update (ClosedExpr TopId) [TopBinding Inert]
  | Jump TopId

mapOutcomeExpr :: (ClosedExpr TopId -> ClosedExpr TopId) -> Outcome -> Outcome
mapOutcomeExpr _ outcome@Stuck{} = outcome
mapOutcomeExpr _ outcome@Jump{}  = outcome
mapOutcomeExpr f (Update e bs) = Update (f e) bs

orIfStuck :: Outcome -> Outcome -> Outcome
Stuck `orIfStuck` r = r
r `orIfStuck` _ = r

type TopEnv = Map TopId (TopBinding Inert)

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
generateTopBindings :: TopEnv -> HList (Binding TopId '[]) out -> State [TopBinding Inert] (HList (Const (Value TopId)) out)
generateTopBindings _ HNil = return HNil
generateTopBindings env (Bind varBndr e :& bs) = do
  e' <- generateTopBinding env varBndr e
  fmap (Const e' :&) (generateTopBindings env bs)

generateTopBinding :: TopEnv -> VarBndr v -> Expr TopId '[] -> State [TopBinding Inert] (Value TopId)
generateTopBinding _ _ (ValE val) = return val
generateTopBinding env varBndr e = do
  let x = freshId env varBndr
  modify (TopBind () x e :)
  return (RefV x)

evalstepExpr :: TopEnv -> ClosedExpr TopId -> Outcome
evalstepExpr env (ValE (PrimV primop) :@ lhs :@ rhs)
  | Just f <- matchNaturalBinOp primop
  = evalstepNaturalBinOp f lhs rhs `orIfStuck`
    mapOutcomeExpr (\lhs' -> ValE (PrimV primop) :@ lhs' :@ rhs) (evalstepExpr env lhs) `orIfStuck`
    mapOutcomeExpr (\rhs' -> ValE (PrimV primop) :@ lhs :@ rhs') (evalstepExpr env rhs)
  | Just f <- matchIntegerBinOp primop
  = evalstepIntegerBinOp f lhs rhs `orIfStuck`
    mapOutcomeExpr (\lhs' -> ValE (PrimV primop) :@ lhs' :@ rhs) (evalstepExpr env lhs) `orIfStuck`
    mapOutcomeExpr (\rhs' -> ValE (PrimV primop) :@ lhs :@ rhs') (evalstepExpr env rhs)
  | Integer_eq <- primop
  = evalstepIntegerEq lhs rhs `orIfStuck`
    mapOutcomeExpr (\lhs' -> ValE (PrimV primop) :@ lhs' :@ rhs) (evalstepExpr env lhs) `orIfStuck`
    mapOutcomeExpr (\rhs' -> ValE (PrimV primop) :@ lhs :@ rhs') (evalstepExpr env rhs)
evalstepExpr env (CaseE e bs)
  | ValE val <- e, isValueHNF val = evalstepCaseOfVal val bs
  | otherwise = mapOutcomeExpr (\e' -> CaseE e' bs) (evalstepExpr env e)
evalstepExpr env (ValE (RefV ref))
  | Just (TopBind _ _ e) <- Map.lookup ref env
  , isExprWHNF e
  = Update (extendExprCtx e) []
  | otherwise = Jump ref
evalstepExpr env (LamE varBndr e1 :@ e2) =
  let (substItem, topBindings) = runState (generateTopBinding env varBndr e2) []
      subst = mkSubst (Const substItem :& HNil)
  in Update (substExpr subst e1) topBindings
evalstepExpr env (e1 :@ e2) =
  mapOutcomeExpr (\e1' -> e1' :@ e2) (evalstepExpr env e1)
evalstepExpr env (LetE bs e) =
  rightIdListAppend bs $
  let (substItems, topBindings) = runState (generateTopBindings env localBindings) []
      localBindings = hmap (substBinding subst) bs
      subst = mkSubst substItems
  in Update (substExpr subst e) topBindings
evalstepExpr _ _ = Stuck

evalstepCaseOfVal :: Value TopId -> Branches TopId '[] -> Outcome
evalstepCaseOfVal val (Branches bs mb) = go bs
  where
    go [] =
      case mb of
        Nothing -> Stuck
        Just (p :-> e) ->
          case p of
            WildP -> Update e []
            VarP{} ->
              let subst = mkSubst (Const val :& HNil)
              in Update (substExpr subst e) []
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
                    in Update (substExpr subst e) []
            else go bs'
        LitP lit'
          | LitV lit <- val,
            Just eqLit <- matchLit lit lit'
          -> if eqLit
            then Update e []
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
  (Natural -> Natural -> Natural) ->
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepNaturalBinOp f lhs rhs
  | ValE (LitV (NatL a)) <- lhs, ValE (LitV (NatL b)) <- rhs
  = Update (ValE (LitV (NatL (f a b)))) []
  | otherwise = Stuck

evalstepIntegerBinOp ::
  (Integer -> Integer -> Integer) ->
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepIntegerBinOp f lhs rhs
  | ValE (LitV (IntL a)) <- lhs, ValE (LitV (IntL b)) <- rhs
  = Update (ValE (LitV (IntL (f a b)))) []
  | otherwise = Stuck

evalstepIntegerEq ::
  ClosedExpr TopId ->
  ClosedExpr TopId ->
  Outcome
evalstepIntegerEq lhs rhs
  | ValE (LitV (IntL a)) <- lhs, ValE (LitV (IntL b)) <- rhs
  = Update (ValE (primBool (a == b))) []
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

gcSweepTopBindings :: [TopBinding GarbageMarked] -> [TopBinding Inert]
gcSweepTopBindings = mapMaybe sweep
  where
    sweep :: TopBinding GarbageMarked -> Maybe (TopBinding Inert)
    sweep (TopBind Live name e) = Just $ TopBind () name e
    sweep (TopBind Dead _ _) = Nothing

gcMarkTopBindings :: TopId -> [TopBinding Inert] -> [TopBinding GarbageMarked]
gcMarkTopBindings root topBinds = map mark topBinds
  where
    mark :: TopBinding Inert -> TopBinding GarbageMarked
    mark (TopBind _ name e) = TopBind (isLive name) name e

    env = mkTopEnv topBinds
    liveSet =
      case getExpr root of
        Nothing -> Set.empty
        Just e -> execState (goExpr e) (Set.singleton root)

    isLive :: TopId -> MarkBit
    isLive name
      | Set.member name liveSet = Live
      | otherwise = Dead

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

gcTopBindings :: TopId -> [TopBinding Inert] -> [TopBinding Inert]
gcTopBindings root = gcSweepTopBindings . gcMarkTopBindings root