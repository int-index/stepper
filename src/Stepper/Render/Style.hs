{-# LANGUAGE ImplicitParams #-}

module Stepper.Render.Style where

import Data.Text (Text)
import qualified GI.Cairo.Render as Cairo

data Color = RGB Double Double Double

setColor :: Color -> Cairo.Render ()
setColor (RGB r g b) = Cairo.setSourceRGB r g b

data Style =
  MkStyle {
    fontFamily :: Text,
    bodyFontSize :: Int,
    backgroundColor :: Color,
    identColor :: Color,
    punctColor :: Color,
    borderColor :: Color,
    borderWidth :: Double
  }

withStyle :: Style -> ((?style :: Style) => r) -> r
withStyle style r = let ?style = style in r