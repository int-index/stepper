{-# LANGUAGE ImplicitParams #-}

module Stepper.Render.Layout where

import qualified GI.Cairo.Render as Cairo
import GHC.Records

import Stepper.Render.Style

data Offset = O { x, y :: Int }

-- pointwise maximum and minimum
offsetmax, offsetmin :: Offset -> Offset -> Offset
offsetmax (O x1 y1) (O x2 y2) = O (max x1 x2) (max y1 y2)
offsetmin (O x1 y1) (O x2 y2) = O (min x1 x2) (min y1 y2)

instance Num Offset where
  O x1 y1 + O x2 y2 = O (x1 + x2) (y1 + y2)
  O x1 y1 - O x2 y2 = O (x1 - x2) (y1 - y2)
  O x1 y1 * O x2 y2 = O (x1 * x2) (y1 * y2)
  negate (O x y) = O (negate x) (negate y)
  fromInteger a = O (fromInteger a) (fromInteger a)
  abs (O x y) = O (abs x) (abs y)
  signum (O x y) = O (signum x) (signum y)

data Extents = E { w, h :: Int }

data Layout =
  L {
    topLeft :: Offset,
    bottomRight :: Offset,
    render :: Offset -> Cairo.Render ()
  }

instance Semigroup Layout where
  layoutUnder <> layoutOver =
    L {
      topLeft     = offsetmin layoutUnder.topLeft layoutOver.topLeft,
      bottomRight = offsetmax layoutUnder.bottomRight layoutOver.bottomRight,
      render = \offset -> do
        layoutUnder.render offset
        layoutOver.render offset
    }

addOffset :: Offset -> Layout -> Layout
addOffset offset layout =
  L {
    topLeft = layout.topLeft + offset,
    bottomRight = layout.bottomRight + offset,
    render = \o -> layout.render (o + offset)
  }

resetOrigin :: Layout -> Layout
resetOrigin layout = addOffset (-layout.topLeft) layout

horiz :: Layout -> Layout -> Layout
horiz layoutLeft layoutRight = layoutLeft <> addOffset offset layoutRight
  where offset = 0 { x = layoutLeft.bottomRight.x - layoutRight.topLeft.x }

vert :: Layout -> Layout -> Layout
vert layoutAbove layoutBelow = layoutAbove <> addOffset offset layoutBelow
  where offset = 0 { y = layoutAbove.bottomRight.y - layoutBelow.topLeft.y }

instance HasField "extents" Layout Extents where
  getField layout = E{
    w = layout.bottomRight.x - layout.topLeft.x,
    h = layout.bottomRight.y - layout.topLeft.y
  }

padded :: Layout -> Layout
padded layout =
  L {
    topLeft = layout.topLeft - 10,
    bottomRight = layout.bottomRight + 10,
    render = layout.render
  }

framed :: Double -> Color -> Layout -> Layout
framed frameWidth frameColor layout =
  L {
    topLeft = layout.topLeft,
    bottomRight = layout.bottomRight,
    render = \offset -> do
      let o = offset + layout.topLeft
          e = layout.extents
      Cairo.setFillRule Cairo.FillRuleEvenOdd
      Cairo.rectangle
        (fromIntegral o.x)
        (fromIntegral o.y)
        (fromIntegral e.w)
        (fromIntegral e.h)
      Cairo.rectangle
        (fromIntegral o.x + frameWidth)
        (fromIntegral o.y + frameWidth)
        (fromIntegral e.w - 2*frameWidth)
        (fromIntegral e.h - 2*frameWidth)
      setColor frameColor
      Cairo.fill
      layout.render offset
  }

centered :: Extents -> Layout -> Layout
centered extents layout =
  L {
    topLeft = 0,
    bottomRight = O{x = extents.w, y = extents.h},
    render = \offset ->
      let
        halfdiff a b = max 0 (a - b) `div` 2
        offset' = O { x = halfdiff extents.w layout.extents.w,
                      y = halfdiff extents.h layout.extents.h }
      in layout.render (offset + offset')
  }