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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import Data.Maybe

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
  case fill extents (map renderTopBinding bs) of
    Nothing -> comic14 "Empty module"
    Just layout -> addOffset (-layout.topLeft) layout

fill :: (?lctx :: LayoutCtx) => Extents -> [Layout] -> Maybe Layout
fill _ [] = Nothing
fill extents (item:items) =
  let (rowLayout, items') = row item items
  in case fill extents items' of
       Nothing -> Just rowLayout
       Just layouts -> Just (rowLayout `vert` layouts)
  where
    row :: Layout -> [Layout] -> (Layout, [Layout])
    row rowLayout [] = (rowLayout, [])
    row rowLayout rowItems@(rowItem : rowItems')
      | xPos' < extents.w = row rowLayout' rowItems'
      | otherwise = (rowLayout, rowItems)
      where
        xPos' = rowLayout'.bottomRight.x
        rowLayout' = rowLayout `horiz` rowItem

renderTopBinding :: (?lctx :: LayoutCtx) => TopBinding -> Layout
renderTopBinding (TopBind v e) =
  padded $ framed $ padded $
    renderTopId v `horiz` comic14 " = " `horiz` renderExpr topPrec HNil e

renderTopId :: (?lctx :: LayoutCtx) => TopId -> Layout
renderTopId (TopIdUser v) = renderIdent v.str
renderTopId (TopIdGen v n) =
  renderIdent v.str `horiz` addOffset subOffset sub
  where
    subOffset = 0{y = sub.extents.h `div` 6}
    sub = comic8 (Text.pack (show n))
    comic8 = ?lctx.mkTextLayout "Comic Sans MS" 8000

renderIdent :: (?lctx :: LayoutCtx) => Text -> Layout
renderIdent v =
  if Char.isAlpha (Text.head v)
  then comic14 v
  else comic14 "(" `horiz` comic14 v `horiz` comic14 ")"

type Prec = Int

framedIf :: Bool -> Layout -> Layout
framedIf True  = framed . padded
framedIf False = id

topPrec, opPrec, appPrec :: Prec
appPrec = 4
opPrec = 2
topPrec = 0

renderExpr :: (?lctx :: LayoutCtx) => Prec -> HList VarBndr ctx -> Expr TopId ctx -> Layout
renderExpr prec ctx (ValE val) = renderValueExpr prec ctx val
renderExpr prec ctx (LamE varBndr@(VB v) e) =
  framedIf (prec > topPrec) $
  (comic14 "\\" `horiz` renderIdent v.str `horiz` comic14 " -> ")
    `vert` renderExpr topPrec (varBndr :& ctx) e
renderExpr prec ctx (e1 :@ e2) =
  framedIf (prec >= appPrec) $
  renderExpr opPrec ctx e1 `horiz` comic14 " " `horiz` renderExpr appPrec ctx e2
renderExpr prec ctx (CaseE e bs) =
  framedIf (prec > topPrec) $
  (comic14 "case " `horiz` renderExpr opPrec ctx e `horiz` comic14 " of")
    `vert` addOffset 0{x=20} (renderBranches ctx bs)
renderExpr prec ctx (LetE bs e) =
  framedIf (prec > topPrec) $
  let varBndrs = hmap getBindingVarBndr bs
      ctx' = varBndrs ++& ctx
  in
    (comic14 "let " `horiz` renderBindings ctx' bs)
    `vert`
    (comic14 "in " `horiz` renderExpr topPrec ctx' e)

renderValueExpr :: (?lctx :: LayoutCtx) => Prec -> HList VarBndr ctx -> ValueExpr TopId ctx -> Layout
renderValueExpr _ _ (RefV v) = renderTopId v
renderValueExpr _ ctx (VarV i) =
  case ctx !!& i of
    VB v -> renderIdent v.str
renderValueExpr _ _ (LitV lit) = renderLit lit
renderValueExpr _ ctx (ConAppV con args) = renderConAppV ctx con args
renderValueExpr _ _ (PrimV primop) = renderPrimOp primop

renderBindings :: forall ctx out. (?lctx :: LayoutCtx) => HList VarBndr ctx -> HList (Binding TopId ctx) out -> Layout
renderBindings ctx = go
  where
    go :: forall out1. HList (Binding TopId ctx) out1 -> Layout
    go HNil = comic14 "{}"
    go (b :& HNil) = renderBinding ctx b
    go (b :& bs) = renderBinding ctx b `vert` go bs

renderBinding :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Binding TopId ctx v -> Layout
renderBinding ctx (Bind (VB v) e) = renderIdent v.str `horiz` comic14 " = " `horiz` renderExpr topPrec ctx e

renderBranches :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Branches TopId ctx -> Layout
renderBranches ctx (Branches bs mb) = foldr1 vert (bs' ++ maybeToList mb')
  where bs' = map (renderBranch ctx) bs
        mb' = fmap (renderBranch ctx) mb

renderBranch :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Branch TopId psort ctx -> Layout
renderBranch ctx (VarP varBndr :-> e) = renderVarBndr varBndr `horiz` comic14 " -> " `horiz` renderExpr topPrec (varBndr :& ctx) e
renderBranch ctx (ConAppP con varBndrs :-> e) = renderConAppP con varBndrs `horiz` comic14 " -> " `horiz` renderExpr topPrec (varBndrs ++& ctx) e
renderBranch ctx (LitP lit :-> e) = renderLit lit `horiz` comic14 " -> " `horiz` renderExpr topPrec ctx e
renderBranch ctx (WildP :-> e) = comic14 "_" `horiz` comic14 " -> " `horiz` renderExpr topPrec ctx e

renderConAppV :: forall ctx. (?lctx :: LayoutCtx) => HList VarBndr ctx -> Con -> [ValueExpr TopId ctx] -> Layout
renderConAppV ctx con args = renderIdent con.str `horiz` comic14 "(" `horiz` go args `horiz` comic14 ")"
  where
    go :: [ValueExpr TopId ctx] -> Layout
    go [] = comic14 ""
    go [arg] = renderValueExpr topPrec ctx arg
    go (arg : args') = renderValueExpr topPrec ctx arg `horiz` comic14 ", " `horiz` go args'

renderConAppP :: (?lctx :: LayoutCtx) => Con -> HList VarBndr out -> Layout
renderConAppP con varBndrs = renderIdent con.str `horiz` comic14 "(" `horiz` go varBndrs `horiz` comic14 ")"
  where
    go :: HList VarBndr out -> Layout
    go HNil = comic14 ""
    go (varBndr :& HNil) = renderVarBndr varBndr
    go (varBndr :& varBndrs') = renderVarBndr varBndr `horiz` comic14 ", " `horiz` go varBndrs'

renderVarBndr :: (?lctx :: LayoutCtx) => VarBndr v -> Layout
renderVarBndr (VB v) = renderIdent v.str

renderLit :: (?lctx :: LayoutCtx) => Lit -> Layout
renderLit (NatL lit) = comic14 (Text.pack (show lit))
renderLit (IntL lit) = comic14 (Text.pack (show lit))
renderLit (FrcL lit) = comic14 (Text.pack (show lit))
renderLit (StrL lit) = comic14 (Text.pack (show lit))
renderLit (ChrL lit) = comic14 (Text.pack (show lit))

renderPrimOp ::  (?lctx :: LayoutCtx) => PrimOp -> Layout
renderPrimOp primop = comic14 modname.str `horiz` comic14 "." `horiz` comic14 name.str
  where (modname, name) = primopName primop