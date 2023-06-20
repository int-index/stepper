module Stepper.Renamer where

import Data.Kind
import Data.IText
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Inductive

import Stepper.Syntax.Parsed
import Stepper.Syntax.Scoped

data RnError =
    RnErrNameNotFound IText
  deriving (Show)

type Renamer = Either RnError

renameModule :: PModule -> Either RnError Module
renameModule = rnModule

rnModule :: PModule -> Renamer Module
rnModule (PMod bs) = do
  -- TODO: check that topIds are unique
  let topIds = Set.fromList [v | PBind v _ <- bs]
  bs' <- traverse (rnBinding topIds) bs
  return (Mod bs')

rnBinding :: Set PVar -> PBinding -> Renamer TopBinding
rnBinding topIds (PBind v e) = do
  e' <- rnExpr topIds HNil e
  return (TopBind v e')

rnExpr :: forall ctx. Set PVar -> HList VarBndr ctx -> PExpr -> Renamer (Expr TopId ctx)
rnExpr topIds = go
  where
    go :: forall ctx1. HList VarBndr ctx1 -> PExpr -> Renamer (Expr TopId ctx1)
    go ctx (PVarE v)
      | Just (MkSome i) <- lookupLocal ctx v = return (VarE i)
      | Set.member v topIds = return (RefE v)
      | otherwise = Left (RnErrNameNotFound v)
    go _ (PConE con) = return (ConE con)
    go _ (PLitE lit) = return (LitE lit)
    go _ (PPrimE primop) = return (PrimE primop)
    go ctx (PLamE v e) =
      rnVarBndr v \varBndr -> do
        e' <- go (varBndr :& ctx) e
        return (LamE varBndr e')
    go ctx (PAppE e1 e2) = do
      e1' <- go ctx e1
      e2' <- go ctx e2
      return (e1' :@ e2')
    go ctx (PCaseE e bs) = do
      e' <- go ctx e
      bs' <- traverse (rnBranch topIds ctx) bs
      return (CaseE e' bs')
    go ctx (PLetE bs e) =
      rnBindingsLHS bs \bs' -> do
        let varBndrs = hmap getHalfRnBindingVarBndr bs'
            ctx' = varBndrs ++& ctx
        bs'' <- htraverse (rnBindingRHS topIds ctx') bs'
        e' <- go ctx' e
        return (LetE bs'' e')

-- LHS renamed, RHS not yet
type HalfRnBinding :: VarInfo -> Type
data HalfRnBinding v where
  HalfRnBind :: VarBndr v -> PExpr -> HalfRnBinding v

rnBindingLHS :: PBinding -> (forall v. HalfRnBinding v -> Renamer r) -> Renamer r
rnBindingLHS (PBind v e) cont = rnVarBndr v \varBndr -> cont (HalfRnBind varBndr e)

rnBindingsLHS :: [PBinding] -> (forall out. HList HalfRnBinding out -> Renamer r) -> Renamer r
rnBindingsLHS [] cont = cont HNil
rnBindingsLHS (b : bs) cont =
  rnBindingLHS b \b' ->
    rnBindingsLHS bs \bs' ->
      cont (b' :& bs')

getHalfRnBindingVarBndr :: HalfRnBinding v -> VarBndr v
getHalfRnBindingVarBndr (HalfRnBind varBndr _) = varBndr

rnBindingRHS :: forall ctx v. Set PVar -> HList VarBndr ctx -> HalfRnBinding v -> Renamer (Binding TopId ctx v)
rnBindingRHS topIds ctx (HalfRnBind varBndr e) = do
  e' <- rnExpr topIds ctx e
  return (Bind varBndr e')

rnBranch :: forall ctx. Set PVar -> HList VarBndr ctx -> PBranch -> Renamer (Branch TopId ctx)
rnBranch topIds ctx (PBr p e) =
  rnPat p \out p' -> do
    e' <- rnExpr topIds (out ++& ctx) e
    return (p' :-> e')

rnPat :: PPat -> (forall out. HList VarBndr out -> Pat out -> Renamer r) -> Renamer r
rnPat (PVarP v) cont =
  rnVarBndr v \varBndr ->
    cont (varBndr :& HNil) (VarP varBndr)
rnPat (PConP con vs) cont =
  rnVarBndrs vs \varBndrs ->
    cont varBndrs (ConP con varBndrs)
rnPat (PLitP lit) cont = cont HNil (LitP lit)
rnPat PWildP cont = cont HNil WildP

rnVarBndr :: PVar -> (forall v. VarBndr v -> Renamer r) -> Renamer r
rnVarBndr v cont = cont (VB v)

rnVarBndrs :: [PVar] -> (forall out. HList VarBndr out -> Renamer r) -> Renamer r
rnVarBndrs [] cont = cont HNil
rnVarBndrs (v:vs) cont =
  rnVarBndr v \varBndr ->
    rnVarBndrs vs \varBndrs ->
      cont (varBndr :& varBndrs)

lookupLocal :: HList VarBndr ctx -> PVar -> Maybe (Some (Index ctx))
lookupLocal HNil _ = Nothing
lookupLocal (VB v :& _) v' | v == v' = Just (MkSome Z)
lookupLocal (_ :& vs) v' = do
  MkSome i <- lookupLocal vs v'
  return (MkSome (S i))