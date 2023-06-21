module Stepper.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map

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
        Update b' -> Just (Mod (setTopBinding b' bs))
        Jump name' -> go name'

setTopBinding :: TopBinding -> [TopBinding] -> [TopBinding]
setTopBinding _ [] = []
setTopBinding b@(TopBind name _) (b'@(TopBind name' _) : bs)
  | name == name' = b : bs
  | otherwise = b' : setTopBinding b bs

data Outcome =
    Stuck
  | Update TopBinding
  | Jump TopId

orIfStuck :: Outcome -> Outcome -> Outcome
Stuck `orIfStuck` r = r
r `orIfStuck` _ = r

evalstepTopBinding :: TopEnv -> TopBinding -> Outcome
evalstepTopBinding env (TopBind name e) = evalstepExpr env (TopBind name) e

type ExprCtx a = Expr TopId a -> TopBinding

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

evalstepExpr :: TopEnv -> ExprCtx ctx -> Expr TopId ctx -> Outcome
evalstepExpr env ctx (PrimE primop :@ lhs :@ rhs)
  | Just f <- matchIntegerBinOp primop
  = evalstepIntegerBinOp ctx f lhs rhs `orIfStuck`
    evalstepExpr env (\lhs' -> ctx (PrimE primop :@ lhs' :@ rhs)) lhs `orIfStuck`
    evalstepExpr env (\rhs' -> ctx (PrimE primop :@ lhs :@ rhs')) rhs
evalstepExpr env ctx (RefE ref)
  | Just (TopBind _ e) <- Map.lookup ref env
  , isWHNF e
  = Update (ctx (extendExprCtx e))
  | otherwise = Jump ref
-- evalstepEpxr ctx (CaseE (LitE _) bs) = ...
-- evalstepExpr ctx (CaseE e bs) = evalstepExpr (\e' -> ctx (CaseE e' bs)) e
evalstepExpr _ _ _ = Stuck

evalstepIntegerBinOp ::
  ExprCtx ctx ->
  (Integer -> Integer -> Integer) ->
  Expr TopId ctx ->
  Expr TopId ctx ->
  Outcome
evalstepIntegerBinOp ctx f lhs rhs
  | LitE (IntL a) <- lhs, LitE (IntL b) <- rhs
  = Update (ctx (LitE (IntL (f a b))))
  | otherwise = Stuck

matchIntegerBinOp :: PrimOp -> Maybe (Integer -> Integer -> Integer)
matchIntegerBinOp primop =
  case primop of
    Integer_add -> Just (+)
    Integer_sub -> Just (-)
    Integer_mul -> Just (*)
    Integer_div -> Just div
    _ -> Nothing