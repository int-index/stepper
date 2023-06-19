module Stepper.Interactive (runInteractiveApp) where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import Data.IORef
import System.IO.Unsafe

import Stepper.Syntax.Scoped
import Stepper.Render

runInteractiveApp :: Module -> IO ()
runInteractiveApp srcMod = do
  Just app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (appActivate app srcMod)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = Text.pack "int-index.stepper"

appActivate :: Gtk.Application -> Module -> IO ()
appActivate app srcMod = do
  window <- Gtk.applicationWindowNew app
  Gtk.setWindowDefaultWidth window 800
  Gtk.setWindowDefaultHeight window 600
  Gtk.setWidgetAppPaintable window True

  fontCacheRef <- newIORef emptyFontCache

  _ <- Gtk.onWidgetDraw window $
    Cairo.renderWithContext $ do
      cairoContext <- Cairo.getContext
      let mkTextLayout :: Text -> Int -> Text -> Layout
          mkTextLayout fontFamily fontSize str = unsafePerformIO do
            Cairo.renderWithContext (createTextLayout fontCacheRef fontFamily fontSize str) cairoContext
      (x1, y1, x2, y2) <- Cairo.clipExtents
      let (w, h) = (x2 - x1, y2 - y1)
      renderBackground w h
      withLayoutCtx LCtx{mkTextLayout} $
        (renderModule (E{w = floor w,h = floor h}) srcMod).render 0
      return True

  Gtk.widgetShow window

renderBackground :: Double -> Double -> Cairo.Render ()
renderBackground w h = do
  Cairo.rectangle 0 0 w h
  Cairo.setSourceRGB 0.1 0.1 0.5
  Cairo.fill