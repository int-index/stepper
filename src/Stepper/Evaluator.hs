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
import qualified Data.List.NonEmpty as NE

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped
import Stepper.BuiltIn (BuiltInStrings(..), builtInStrings)

gcMark :: Module Eval -> Maybe (Module GarbageMarked)
gcMark (Mod bs stk _)
  | all isLive bs = Nothing
  | otherwise     = Just (Mod bs stk liveSet)
  where
    liveSet = gcComputeLiveSet stk bs
    isLive b = Set.member (getTopBindingId b) liveSet

gcSweep :: Module GarbageMarked -> Module Eval
gcSweep (Mod bs stk liveSet) = Mod (filter isLive bs) stk ()
  where
    isLive b = Set.member (getTopBindingId b) liveSet

evalStart :: Module Inert -> TopId -> Maybe (Module Eval)
evalStart (Mod bs _ _) name = do
  TopBind _ e <- lookupTopBinding name bs
  if isExprWHNF e
    then Nothing  -- nothing to evaluate
    else Just $ Mod bs (NE.singleton name) ()

data EvalResult =
    EvalStuck                  -- can't make any progress
  | EvalPushed  (Module Eval)  -- pushed to the stack
  | EvalReduced (Module Eval)  -- reduced an expression, more to do
  | EvalReducedLast (Module Inert) -- reduced the last expression, nothing to do

evalStep :: Module Eval -> EvalResult
evalStep (Mod bs stk _) =
  if Set.member name visited
  then EvalStuck
  else
    case evalstepExpr env e of
      Stuck -> EvalStuck
      Jump name' -> EvalPushed (Mod bs (NE.cons name' stk) ())
      Update e' newBindings ->
        let
          bs'  = updateTopBinding name e' bs ++ newBindings
          stk'
            | isExprWHNF e' = NE.tail stk    -- the expression was reduced, pop from the stack
            | otherwise     = NE.toList stk  -- more to do in this expr
        in
          case NE.nonEmpty stk' of
            Nothing    -> EvalReducedLast (Mod bs' () ())
            Just stk'' -> EvalReduced (Mod bs' stk'' ())
  where
    TopBind _ e = unwrapBindingLookup (Map.lookup name env)
    env = mkTopEnv bs
    visited = Set.fromList (NE.tail stk)
    name = NE.head stk

unwrapBindingLookup :: Maybe a -> a
unwrapBindingLookup Nothing  = error "impossible: reference to a non-existent binding"  -- the renamer should have ruled those out
unwrapBindingLookup (Just x) = x

mkTopEnv :: [TopBinding] -> Map TopId TopBinding
mkTopEnv bs = Map.fromList [ (name, b) | b@(TopBind name _) <- bs ]

lookupTopBinding :: TopId -> [TopBinding] -> Maybe TopBinding
lookupTopBinding _ [] = Nothing
lookupTopBinding name (b : bs)
  | getTopBindingId b == name = Just b
  | otherwise = lookupTopBinding name bs

updateTopBinding :: TopId -> ClosedExpr TopId -> [TopBinding] -> [TopBinding]
updateTopBinding _ _ [] = []
updateTopBinding name e (b'@(TopBind name' _) : bs)
  | name == name' = TopBind name e : bs
  | otherwise = b' : updateTopBinding name e bs

data Outcome =
    Stuck
  | Update (ClosedExpr TopId) [TopBinding]
  | Jump TopId

wrapOutcomeExpr :: ExprWrapper TopId -> Outcome -> Outcome
wrapOutcomeExpr _ outcome@Stuck{} = outcome
wrapOutcomeExpr _ outcome@Jump{}  = outcome
wrapOutcomeExpr ew (Update e bs) = Update (wrapExpr ew e) bs

type TopEnv = Map TopId TopBinding

isValueHNF :: Value TopId -> Bool
isValueHNF v =
  case v of
    RefV{}    -> False
    LitV{}    -> True
    ConAppV{} -> True

isExprWHNF :: ClosedExpr TopId -> Bool
isExprWHNF e =
  case e of
    ValE v  -> isValueHNF v
    LamE{}  -> True
    _ :@ _  -> False
    PrimCallE{} -> False
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

evalstepExpr :: TopEnv -> ClosedExpr TopId -> Outcome
evalstepExpr _ (PrimCallE primop [lhs, rhs])
  | Just f <- matchNaturalBinOp primop = evalstepNaturalBinOp f lhs rhs
  | Just f <- matchIntegerBinOp primop = evalstepIntegerBinOp f lhs rhs
  | Integer_eq <- primop = evalstepIntegerEq lhs rhs
evalstepExpr env (CaseE e bs)
  | ValE val <- e, isValueHNF val = evalstepCaseOfVal val bs
  | otherwise = wrapOutcomeExpr (CaseEW bs) (evalstepExpr env e)
evalstepExpr env (ValE (RefV ref))
  | Just (TopBind _ e) <- Map.lookup ref env
  , isExprWHNF e
  = Update (extendExprCtx e) []
  | otherwise = Jump ref
evalstepExpr env (LamE varBndr e1 :@ e2) =
  let (substItem, topBindings) = runState (generateTopBinding env varBndr e2) []
      subst = mkSubst (Const substItem :& HNil)
  in Update (substExpr subst e1) topBindings
evalstepExpr env (e1 :@ e2) =
  wrapOutcomeExpr (ArgEW e2) (evalstepExpr env e1)
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

gcComputeLiveSet :: NE.NonEmpty TopId -> [TopBinding] -> LiveSet GarbageMarked
gcComputeLiveSet roots topBinds = liveSet
  where
    env = mkTopEnv topBinds
    liveSet = execState (traverse_ goExpr rootExprs) visited
      where
        rootExprs = fmap getExpr roots
        visited = Set.fromList (NE.toList roots)

    getExpr :: TopId -> ClosedExpr TopId
    getExpr name = getTopBindingExpr (unwrapBindingLookup (Map.lookup name env))

    goExpr :: Expr TopId ctx -> State (Set TopId) ()
    goExpr (ValE e) = goValueExpr e
    goExpr (e1 :@ e2) = do
      goExpr e1
      goExpr e2
    goExpr (PrimCallE _ args) = traverse_ goExpr args
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
        goExpr (getExpr ref)
    goValueExpr (ConAppV _ args) = traverse_ goValueExpr args
    goValueExpr VarV{} = return ()
    goValueExpr LitV{} = return ()