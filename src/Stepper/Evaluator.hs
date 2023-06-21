module Stepper.Evaluator where

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped

evalstep :: Module -> TopId -> Maybe Module  -- Nothing <=> nothing to reduce
evalstep (Mod bs) entryPoint = do
  b <- lookupTopBinding entryPoint bs
  case evalstepTopBinding b of
    Nothing -> Nothing
    Just b' -> Just (Mod (setTopBinding b' bs))

lookupTopBinding :: TopId -> [TopBinding] -> Maybe TopBinding
lookupTopBinding _ [] = Nothing
lookupTopBinding name (b@(TopBind name' _) : bs)
  | name == name' = Just b
  | otherwise = lookupTopBinding name bs

setTopBinding :: TopBinding -> [TopBinding] -> [TopBinding]
setTopBinding _ [] = []
setTopBinding b@(TopBind name _) (b'@(TopBind name' _) : bs)
  | name == name' = b : bs
  | otherwise = b' : setTopBinding b bs

evalstepTopBinding :: TopBinding -> Maybe TopBinding
evalstepTopBinding (TopBind name e) = do
  e' <- evalstepExpr e
  return (TopBind name e')

evalstepExpr :: ClosedExpr TopId -> Maybe (ClosedExpr TopId)
evalstepExpr (PrimE primop :@ lhs :@ rhs)
  | Just f <- matchIntegerBinOp primop
  = evalstepIntegerBinOp f primop lhs rhs
evalstepExpr _ = Nothing

matchIntegerBinOp :: PrimOp -> Maybe (Integer -> Integer -> Integer)
matchIntegerBinOp primop =
  case primop of
    Integer_add -> Just (+)
    Integer_sub -> Just (-)
    Integer_mul -> Just (*)
    Integer_div -> Just div
    _ -> Nothing

evalstepIntegerBinOp ::
  (Integer -> Integer -> Integer) ->
  PrimOp -> ClosedExpr TopId -> ClosedExpr TopId -> Maybe (ClosedExpr TopId)
evalstepIntegerBinOp f primop lhs rhs
  | Just lhs' <- evalstepExpr lhs = Just (PrimE primop :@ lhs' :@ rhs)
  | Just rhs' <- evalstepExpr rhs = Just (PrimE primop :@ lhs :@ rhs')
  | LitE (IntL a) <- lhs, LitE (IntL b) <- rhs =
      Just (LitE (IntL (f a b)))
  | otherwise = Nothing