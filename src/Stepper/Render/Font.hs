module Stepper.Render.Font where

import Data.Text
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo
import Control.Monad.IO.Class
import Data.IORef

import Stepper.Render.Layout
import Stepper.Render.Style

createFontDescription :: IORef FontCache -> Text -> Int -> IO Pango.FontDescription
createFontDescription fontCacheRef fontFamily fontSize = do
  fontCache0 <- readIORef fontCacheRef
  let k = (fontFamily, fontSize)
  case Map.lookup k fontCache0.fontDescriptionCache of
    Just fontDescription -> return fontDescription
    Nothing -> do
      -- putStrLn $ "createFontDescription: cache miss " ++ show k
      fontDescription <- Pango.fontDescriptionNew
      Pango.fontDescriptionSetFamily fontDescription fontFamily
      Pango.fontDescriptionSetSize fontDescription (fromIntegral fontSize)
      Pango.fontDescriptionSetWeight fontDescription Pango.WeightNormal
      atomicModifyIORef' fontCacheRef \fontCache1 ->
        (
          fontCache1 { fontDescriptionCache =
            Map.insert k fontDescription fontCache1.fontDescriptionCache
          },
          fontDescription
        )

toPixels :: Int32 -> Int
toPixels a = fromIntegral (a `div` Pango.SCALE)

createTextLayout :: IORef FontCache -> Text -> Int -> Text -> Cairo.Render (Color -> Layout)
createTextLayout fontCacheRef fontFamily fontSize str = do
  fontCache0 <- liftIO $ readIORef fontCacheRef
  let k = (fontFamily, fontSize, str)
  case Map.lookup k fontCache0.textLayoutCache of
    Just textLayout -> return textLayout
    Nothing -> do
      -- liftIO $ putStrLn $ "createTextLayout: cache miss " ++ show k
      fontDescription <- liftIO $ createFontDescription fontCacheRef fontFamily fontSize
      pangoLayout <- Cairo.getContext >>= PangoCairo.createLayout
      Pango.layoutSetText pangoLayout str (-1)
      Pango.layoutSetFontDescription pangoLayout (Just fontDescription)
      (_logicalExtents, inkExtents) <- Pango.layoutGetExtents pangoLayout
      inkW <- Pango.getRectangleWidth inkExtents
      inkH <- Pango.getRectangleHeight inkExtents
      let extents = E{w = toPixels inkW, h = toPixels inkH}
      pangoBaseline <- Pango.layoutGetBaseline pangoLayout
      let baseline = toPixels pangoBaseline
      let textLayout color =
            addOffset 0{ y = -baseline } $
            L{
              topLeft = 0,
              bottomRight = O{x = extents.w, y = extents.h},
              render = \offset -> do
                cairoContext <- Cairo.getContext
                Cairo.moveTo (fromIntegral offset.x) (fromIntegral offset.y)
                setColor color
                PangoCairo.showLayout cairoContext pangoLayout
            }
      liftIO $ atomicModifyIORef' fontCacheRef \fontCache1 ->
        (
          fontCache1 { textLayoutCache =
            Map.insert k textLayout fontCache1.textLayoutCache
          },
          textLayout
        )

data FontCache =
  FC {
    fontDescriptionCache :: Map (Text, Int) Pango.FontDescription,
    textLayoutCache :: Map (Text, Int, Text) (Color -> Layout)
  }

emptyFontCache :: FontCache
emptyFontCache = FC Map.empty Map.empty