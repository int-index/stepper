module Stepper.Interactive (runInteractiveApp) where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import Data.IORef
import System.IO.Unsafe
import Control.Monad

import Stepper.Syntax.Scoped
import Stepper.Render
import Stepper.Evaluator

data AppState =
  MkAppState {
    step :: Int,
    mod :: Module
  }

runInteractiveApp :: Module -> IO ()
runInteractiveApp srcMod = do
  appStateRef <- newIORef MkAppState{step = 0, mod = srcMod}
  Just app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (appActivate app appStateRef)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = Text.pack "int-index.stepper"

appActivate :: Gtk.Application -> IORef AppState -> IO ()
appActivate app appStateRef = do
  window <- Gtk.applicationWindowNew app
  Gtk.setWindowDefaultWidth window 800
  Gtk.setWindowDefaultHeight window 600
  Gtk.setWidgetAppPaintable window True

  fontCacheRef <- newIORef emptyFontCache

  _ <- Gtk.onWidgetDraw window $
    Cairo.renderWithContext $ do
      appState <- Cairo.liftIO $ readIORef appStateRef
      cairoContext <- Cairo.getContext
      let mkTextLayout :: Text -> Int -> Text -> Layout
          mkTextLayout fontFamily fontSize str = unsafePerformIO do
            Cairo.renderWithContext (createTextLayout fontCacheRef fontFamily fontSize str) cairoContext
      (x1, y1, x2, y2) <- Cairo.clipExtents
      let (w, h) = (x2 - x1, y2 - y1)
      renderBackground w h
      withLayoutCtx LCtx{mkTextLayout} $
        (
          renderStep appState.step <>
          renderModule (E{w = floor w, h = floor h}) appState.mod
        ).render 0
      return True

  _ <- Gtk.onWidgetKeyPressEvent window \eventKey -> do
    keyval <- Gdk.getEventKeyKeyval eventKey
    case keyval of
      Gdk.KEY_space -> do
        updated <-
          atomicModifyIORef' appStateRef \appState ->
            case evalstep appState.mod of
              Nothing -> (appState, False)
              Just mod' ->
                (appState{
                  step = appState.step + 1,
                  mod = mod'
                }, True)
        when updated $ Gtk.widgetQueueDraw window
        return updated
      _ -> return False

  Gtk.widgetShow window

renderBackground :: Double -> Double -> Cairo.Render ()
renderBackground w h = do
  Cairo.rectangle 0 0 w h
  Cairo.setSourceRGB 0.1 0.1 0.5
  Cairo.fill