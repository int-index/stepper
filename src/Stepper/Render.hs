{-# LANGUAGE ImplicitParams #-}

module Stepper.Render (
  Layout(..),
  Extents(..),
  emptyFontCache,
  createTextLayout,
  LayoutCtx(..),
  withLayoutCtx,
  renderStep,
  renderModule,
) where

import Data.Inductive
import Data.IText
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char

import Stepper.Syntax.Basic
import Stepper.Syntax.Scoped
import Stepper.Render.Layout
import Stepper.Render.Font

newtype LayoutCtx = LCtx { mkTextLayout :: Text -> Int -> Text -> Layout }

withLayoutCtx :: LayoutCtx -> ((?lctx :: LayoutCtx) => r) -> r
withLayoutCtx lctx r = let ?lctx = lctx in r

comic14 :: (?lctx :: LayoutCtx) => Text -> Layout
comic14 = ?lctx.mkTextLayout "Comic Sans MS" 14000

renderStep :: (?lctx :: LayoutCtx) => Int -> Layout
renderStep n =
  let layout = comic14 "Step: " `horiz` comic14 (Text.pack (show n))
  in addOffset (-layout.topLeft) layout

renderModule :: (?lctx :: LayoutCtx) => Extents -> Module -> Layout
renderModule extents (Mod bs) =
  centered extents $ foldr1 vert (map renderTopBinding bs)

renderTopBinding :: (?lctx :: LayoutCtx) => TopBinding -> Layout
renderTopBinding (TopBind v e) =
  padded $ -- framed $
    renderIdent v `horiz` comic14 " = " `horiz` renderExpr topPrec HNil e

renderIdent :: (?lctx :: LayoutCtx) => IText -> Layout
renderIdent v =
  if Char.isAlpha (Text.head v.str)
  then comic14 v.str
  else comic14 "(" `horiz` comic14 v.str `horiz` comic14 ")"

type Prec = Int

framedIf :: Bool -> Layout -> Layout
framedIf True  = framed . padded
framedIf False = id

topPrec, opPrec, appPrec :: Prec
appPrec = 4
opPrec = 2
topPrec = 0

renderExpr :: (?lctx :: LayoutCtx) => Prec -> HList VarBndr ctx -> Expr TopId ctx -> Layout
renderExpr _ _ (RefE v) = renderIdent v
renderExpr _ ctx (VarE i) =
  case ctx !!& i of
    VB v -> renderIdent v
renderExpr _ _ (ConE con) = renderIdent con
renderExpr _ _ (LitE lit) = renderLit lit
renderExpr _ _ (PrimE primop) = renderPrimOp primop
renderExpr prec ctx (LamE varBndr@(VB v) e) =
  framedIf (prec > topPrec) $
  (comic14 "\\" `horiz` renderIdent v `horiz` comic14 " -> ")
    `vert` renderExpr topPrec (varBndr :& ctx) e
renderExpr prec ctx (e1 :@ e2) =
  framedIf (prec >= appPrec) $
  renderExpr opPrec ctx e1 `horiz` comic14 " " `horiz` renderExpr appPrec ctx e2
renderExpr prec ctx (CaseE e bs) =
  framedIf (prec > topPrec) $
  (comic14 "case " `horiz` renderExpr topPrec ctx e `horiz` comic14 " of")
    `vert` addOffset 0{x=20} (renderBranches ctx bs)
renderExpr prec ctx (LetE bs e) =
  framedIf (prec > topPrec) $
  let varBndrs = hmap getBindingVarBndr bs
      ctx' = varBndrs ++& ctx
  in
    (comic14 "let " `horiz` renderBindings ctx' bs)
    `vert`
    (comic14 "in " `horiz` renderExpr topPrec ctx' e)

renderBindings :: forall ctx out. (?lctx :: LayoutCtx) => HList VarBndr ctx -> HList (Binding TopId ctx) out -> Layout
renderBindings ctx = go
  where
    go :: forall out1. HList (Binding TopId ctx) out1 -> Layout
    go HNil = comic14 "{}"
    go (b :& HNil) = renderBinding ctx b
    go (b :& bs) = renderBinding ctx b `vert` go bs

renderBinding :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Binding TopId ctx v -> Layout
renderBinding ctx (Bind (VB v) e) = renderIdent v `horiz` comic14 " = " `horiz` renderExpr topPrec ctx e

renderBranches :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> [Branch TopId ctx] -> Layout
renderBranches ctx bs = foldr1 vert (map (renderBranch ctx) bs)

renderBranch :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Branch TopId ctx -> Layout
renderBranch ctx (LitP lit :-> e) = renderLit lit `horiz` comic14 " -> " `horiz` renderExpr topPrec ctx e
renderBranch ctx (WildP :-> e) = comic14 "_" `horiz` comic14 " -> " `horiz` renderExpr topPrec ctx e
renderBranch _ (_ :-> _) = comic14 "todo:branch"

renderLit :: (?lctx :: LayoutCtx) => Lit -> Layout
renderLit (NatL lit) = comic14 (Text.pack (show lit))
renderLit (IntL lit) = comic14 (Text.pack (show lit))
renderLit (FrcL lit) = comic14 (Text.pack (show lit))
renderLit (StrL lit) = comic14 (Text.pack (show lit))
renderLit (ChrL lit) = comic14 (Text.pack (show lit))

renderPrimOp ::  (?lctx :: LayoutCtx) => PrimOp -> Layout
renderPrimOp primop = comic14 modname.str `horiz` comic14 "." `horiz` comic14 name.str
  where (modname, name) = primopName primop