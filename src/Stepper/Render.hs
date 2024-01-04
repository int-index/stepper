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
import Stepper.Render.Style

data LayoutCtx =
  LCtx {
    style :: Style,
    mkTextLayout :: Text -> Int -> Text -> Color -> Layout
  }

withLayoutCtx :: LayoutCtx -> ((?lctx :: LayoutCtx) => r) -> r
withLayoutCtx lctx r = let ?lctx = lctx in r

ident :: (?lctx :: LayoutCtx) => Text -> Layout
ident str = ?lctx.mkTextLayout ?lctx.style.fontFamily ?lctx.style.bodyFontSize str ?lctx.style.identColor

localIdent :: (?lctx :: LayoutCtx) => Text -> Layout
localIdent str = ?lctx.mkTextLayout ?lctx.style.fontFamily ?lctx.style.bodyFontSize str ?lctx.style.localIdentColor

punct :: (?lctx :: LayoutCtx) => Text -> Layout
punct str = ?lctx.mkTextLayout ?lctx.style.fontFamily ?lctx.style.bodyFontSize str ?lctx.style.punctColor

renderStep :: (?lctx :: LayoutCtx) => Int -> Layout
renderStep n =
  let layout = punct "Step: " `horiz` punct (Text.pack (show n))
  in addOffset (-layout.topLeft) layout

renderModule :: (?lctx :: LayoutCtx) => Extents -> Module -> Layout
renderModule extents (Mod bs) =
  case fill extents (map renderTopBinding bs) of
    Nothing -> punct "Empty module"
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
  withStyle ?lctx.style $
  padded $ framed $ padded $
    renderTopId v `horiz` punct " = " `horiz` renderExpr topPrec HNil e

renderTopId :: (?lctx :: LayoutCtx) => TopId -> Layout
renderTopId (TopIdUser v) = renderPrefix ident v.str
renderTopId (TopIdGen v n) =
  renderPrefix ident v.str `horiz` addOffset subOffset sub
  where
    subOffset = 0{y = sub.extents.h `div` 6}
    sub =
      ?lctx.mkTextLayout
        ?lctx.style.fontFamily
        ((?lctx.style.bodyFontSize * 3) `div` 5)
        (Text.pack (show n))
        ?lctx.style.identColor

renderPrefix :: (?lctx :: LayoutCtx) => (Text -> Layout) -> Text -> Layout
renderPrefix f v =
  if Char.isAlpha (Text.head v)
  then f v
  else punct "(" `horiz` f v `horiz` punct ")"

type Prec = Int

framedIf :: (?style :: Style) => Bool -> Layout -> Layout
framedIf True  = framed . padded
framedIf False = id

topPrec, opPrec, appPrec :: Prec
appPrec = 4
opPrec = 2
topPrec = 0

renderExpr :: (?lctx :: LayoutCtx) => Prec -> HList VarBndr ctx -> Expr TopId ctx -> Layout
renderExpr prec ctx (ValE val) = renderValueExpr prec ctx val
renderExpr prec ctx (LamE varBndr@(VB v) e) =
  withStyle ?lctx.style $
  framedIf (prec > topPrec) $
  (punct "λ" `horiz` renderPrefix localIdent v.str `horiz` punct " → ")
    `vert` renderExpr topPrec (varBndr :& ctx) e
renderExpr prec ctx (e1 :@ e2) =
  withStyle ?lctx.style $
  framedIf (prec >= appPrec) $
  renderExpr opPrec ctx e1 `horiz` ident " " `horiz` renderExpr appPrec ctx e2
renderExpr prec ctx (CaseE e bs) =
  withStyle ?lctx.style $
  framedIf (prec > topPrec) $
  (punct "case " `horiz` renderExpr opPrec ctx e `horiz` punct " of")
    `vert` addOffset 0{x=20} (renderBranches ctx bs)
renderExpr prec ctx (LetE bs e) =
  withStyle ?lctx.style $
  framedIf (prec > topPrec) $
  let varBndrs = hmap getBindingVarBndr bs
      ctx' = varBndrs ++& ctx
  in
    (punct "let " `horiz` renderBindings ctx' bs)
    `vert`
    (punct "in " `horiz` renderExpr topPrec ctx' e)

renderValueExpr :: (?lctx :: LayoutCtx) => Prec -> HList VarBndr ctx -> ValueExpr TopId ctx -> Layout
renderValueExpr _ _ (RefV v) = renderTopId v
renderValueExpr _ ctx (VarV i) =
  case ctx !!& i of
    VB v -> renderPrefix localIdent v.str
renderValueExpr _ _ (LitV lit) = renderLit lit
renderValueExpr _ ctx (ConAppV con args) = renderConAppV ctx con args
renderValueExpr _ _ (PrimV primop) = renderPrimOp primop

renderBindings :: forall ctx out. (?lctx :: LayoutCtx) => HList VarBndr ctx -> HList (Binding TopId ctx) out -> Layout
renderBindings ctx = go
  where
    go :: forall out1. HList (Binding TopId ctx) out1 -> Layout
    go HNil = punct "{}"
    go (b :& HNil) = renderBinding ctx b
    go (b :& bs) = renderBinding ctx b `vert` go bs

renderBinding :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Binding TopId ctx v -> Layout
renderBinding ctx (Bind (VB v) e) = renderPrefix localIdent v.str `horiz` punct " = " `horiz` renderExpr topPrec ctx e

renderBranches :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Branches TopId ctx -> Layout
renderBranches ctx (Branches bs mb) = foldr1 vert (bs' ++ maybeToList mb')
  where bs' = map (renderBranch ctx) bs
        mb' = fmap (renderBranch ctx) mb

renderBranch :: (?lctx :: LayoutCtx) => HList VarBndr ctx -> Branch TopId psort ctx -> Layout
renderBranch ctx (VarP varBndr :-> e) = renderVarBndr varBndr `horiz` punct " → " `horiz` renderExpr topPrec (varBndr :& ctx) e
renderBranch ctx (ConAppP con varBndrs :-> e) = renderConAppP con varBndrs `horiz` punct " → " `horiz` renderExpr topPrec (varBndrs ++& ctx) e
renderBranch ctx (LitP lit :-> e) = renderLit lit `horiz` punct " → " `horiz` renderExpr topPrec ctx e
renderBranch ctx (WildP :-> e) = punct "_" `horiz` punct " → " `horiz` renderExpr topPrec ctx e

renderConAppV :: forall ctx. (?lctx :: LayoutCtx) => HList VarBndr ctx -> Con -> [ValueExpr TopId ctx] -> Layout
renderConAppV ctx con args = renderPrefix ident con.str `horiz` punct "(" `horiz` go args `horiz` punct ")"
  where
    go :: [ValueExpr TopId ctx] -> Layout
    go [] = ident ""
    go [arg] = renderValueExpr topPrec ctx arg
    go (arg : args') = renderValueExpr topPrec ctx arg `horiz` punct ", " `horiz` go args'

renderConAppP :: (?lctx :: LayoutCtx) => Con -> HList VarBndr out -> Layout
renderConAppP con varBndrs = renderPrefix ident con.str `horiz` punct "(" `horiz` go varBndrs `horiz` punct ")"
  where
    go :: HList VarBndr out -> Layout
    go HNil = ident ""
    go (varBndr :& HNil) = renderVarBndr varBndr
    go (varBndr :& varBndrs') = renderVarBndr varBndr `horiz` punct ", " `horiz` go varBndrs'

renderVarBndr :: (?lctx :: LayoutCtx) => VarBndr v -> Layout
renderVarBndr (VB v) = renderPrefix localIdent v.str

renderLit :: (?lctx :: LayoutCtx) => Lit -> Layout
renderLit (NatL lit) = ident (Text.pack (show lit))
renderLit (IntL lit) = ident (Text.pack (show lit))
renderLit (FrcL lit) = ident (Text.pack (show lit))
renderLit (StrL lit) = ident (Text.pack (show lit))
renderLit (ChrL lit) = ident (Text.pack (show lit))

renderPrimOp ::  (?lctx :: LayoutCtx) => PrimOp -> Layout
renderPrimOp primop = ident modname.str `horiz` punct "." `horiz` ident name.str
  where (modname, name) = primopName primop