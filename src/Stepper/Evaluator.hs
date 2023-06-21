{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Stepper.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Const
import Data.Inductive

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped
import Data.Bifunctor

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
  in Update (ctx (substExpr (\case Z -> RefE x) e1)) [TopBind x e2]
evalstepExpr env ctx (e1 :@ e2) =
  evalstepExpr env (\e1' -> ctx (e1' :@ e2)) e1
-- evalstepEpxr ctx (CaseE (LitE _) bs) = ...
-- evalstepExpr ctx (CaseE e bs) = evalstepExpr (\e' -> ctx (CaseE e' bs)) e
evalstepExpr _ _ _ = Stuck

substExpr :: forall ctx ctx' ref . (forall t . Index ctx t -> Expr ref ctx') -> Expr ref ctx -> Expr ref ctx'
substExpr subst (VarE i) = subst i
substExpr subst (RefE r) = RefE r
substExpr subst (ConE c) = ConE c
substExpr subst (LitE l) = LitE l
substExpr subst (PrimE p) = PrimE p
substExpr subst (f :@ x) = substExpr subst f :@ substExpr subst x
substExpr subst (LamE varBndr e) = LamE varBndr $ substExpr (\case Z -> VarE Z; S n -> substExpr (VarE . S) (subst n)) e
substExpr subst (CaseE s c) = CaseE (substExpr subst s) $ fmap (substBranch subst) c
substExpr subst (LetE (h :: HList _ out) e) = LetE (hmap (\(Bind b e) -> Bind b $ substExpr (shiftN @out @ctx subst h) e) h) $ substExpr (shiftN @out @ctx subst h) e

substBranch :: (forall t . Index ctx t -> Expr ref ctx') -> Branch ref ctx -> Branch ref ctx'
substBranch subst (p :-> e) = p :-> substExpr (shiftN subst $ patVarBndrs p) e

shiftN :: forall out ctx ctx' ref t f . (forall t . Index ctx t -> (Expr ref ctx')) -> HList f out -> Index (out ++ ctx) t -> (Expr ref (out ++ ctx'))
shiftN subst HNil Z = subst Z
shiftN subst HNil (S n) = shiftN (subst . S) HNil n
shiftN subst (_ :& _) Z = VarE Z
shiftN subst (_ :& xs) (S n) = substExpr (VarE . S) (shiftN subst xs n)

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
