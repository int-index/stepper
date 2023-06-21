module Stepper.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Const
import Data.Inductive

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

freshId :: TopEnv -> VarBndr v -> TopId
freshId env (VB x) =
  case [n | (TopIdGen x' n, _) <- Map.toDescList env, x == x'] of
    []  -> TopIdGen x 0
    n:_ -> TopIdGen x (n + 1)

evalstepExpr :: TopEnv -> ExprCtx -> ClosedExpr TopId -> Outcome
evalstepExpr env ctx (PrimE primop :@ lhs :@ rhs)
  | Just f <- matchIntegerBinOp primop
  = evalstepIntegerBinOp ctx f lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (PrimE primop :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (PrimE primop :@ lhs :@ rhs')) rhs
evalstepExpr env ctx (RefE ref)
  | Just (TopBind _ e) <- Map.lookup ref env
  , isWHNF e
  = Update (ctx (extendExprCtx e)) []
  | otherwise = Jump ref
evalstepExpr env ctx (LamE varBndr e1 :@ e2) =
  let x = freshId env varBndr
  in Update (ctx (substExpr (Const (RefE x) :& HNil) e1)) [TopBind x e2]
evalstepExpr env ctx (e1 :@ e2) =
  evalstepExpr env (\e1' -> ctx (e1' :@ e2)) e1
-- evalstepEpxr ctx (CaseE (LitE _) bs) = ...
-- evalstepExpr ctx (CaseE e bs) = evalstepExpr (\e' -> ctx (CaseE e' bs)) e
evalstepExpr _ _ _ = Stuck

substExpr :: HList (Const (Expr ref ctx')) ctx -> Expr ref ctx -> Expr ref ctx'
substExpr subst (VarE i) = getConst (subst !!& i)
substExpr subst (LamE varBndr e) =
  LamE varBndr (substExpr (Const (VarE Z) :& hmap (error "todo: substExpr.shift") subst) e)

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

matchIntegerBinOp :: PrimOp -> Maybe (Integer -> Integer -> Integer)
matchIntegerBinOp primop =
  case primop of
    Integer_add -> Just (+)
    Integer_sub -> Just (-)
    Integer_mul -> Just (*)
    Integer_div -> Just div
    _ -> Nothing