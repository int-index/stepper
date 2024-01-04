{-# LANGUAGE ImplicitParams #-}

module Stepper.Interactive (runInteractiveApp) where

import Data.IText
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
import Stepper.Render.Style
import Stepper.Render.Layout (vert)
import Stepper.Evaluator

data AppState =
  MkAppState {
    step :: Int,
    mod :: Module,
    entryPoint :: IText
  }

appStateStep :: AppState -> (AppState, Bool)
appStateStep appState =
  case evalstep appState.mod (TopIdUser appState.entryPoint) of
    Nothing -> (appState, False)
    Just mod' ->
      (appState{
        step = appState.step + 1,
        mod = mod'
      }, True)

runInteractiveApp :: Module -> IText -> IO ()
runInteractiveApp srcMod entryPoint = do
  appStateRef <- newIORef MkAppState{step = 0, mod = srcMod, entryPoint}
  app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (appActivate app appStateRef)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = Text.pack "int-index.stepper"

appActivate :: Gtk.Application -> IORef AppState -> IO ()
appActivate app appStateRef = do
  let ?style = MkStyle {
        fontFamily      = "Noto Sans",
        bodyFontSize    = 14000,
        backgroundColor = RGB 0.11 0.11 0.11,
        identColor    = RGB 0.88 0.88 0.88,
        punctColor    = RGB 0.60 0.60 0.60,
        borderColor   = RGB 0.44 0.44 0.44,
        borderWidth   = 2
      }

  window <- Gtk.applicationWindowNew app
  Gtk.setWindowDefaultWidth window 800
  Gtk.setWindowDefaultHeight window 600

  drawingArea <- createDrawingArea (readIORef appStateRef)
  Gtk.windowSetChild window (Just drawingArea)

  eventControllerKey <- createEventControllerKey appStateRef (Gtk.widgetQueueDraw drawingArea)
  Gtk.widgetAddController window eventControllerKey

  Gtk.windowPresent window

createDrawingArea :: (?style :: Style) => IO AppState -> IO Gtk.DrawingArea
createDrawingArea readAppState = do
  fontCacheRef <- newIORef emptyFontCache
  drawingArea <- Gtk.drawingAreaNew
  Gtk.drawingAreaSetDrawFunc drawingArea $
    Just $ \_ ctx _ _ ->
    flip Cairo.renderWithContext ctx $ do
      appState <- Cairo.liftIO readAppState
      cairoContext <- Cairo.getContext
      let mkTextLayout :: Text -> Int -> Text -> Color -> Layout
          mkTextLayout fontFamily fontSize str color = unsafePerformIO do
            Cairo.renderWithContext (createTextLayout color fontCacheRef fontFamily fontSize str) cairoContext
      (x1, y1, x2, y2) <- Cairo.clipExtents
      let (w, h) = (x2 - x1, y2 - y1)
      renderBackground w h
      withLayoutCtx LCtx{style = ?style, mkTextLayout} $
        (
          renderStep appState.step `vert`
          renderModule (E{w = floor w, h = floor h}) appState.mod
        ).render 0
  return drawingArea

createEventControllerKey :: IORef AppState -> IO () -> IO Gtk.EventControllerKey
createEventControllerKey appStateRef queueRedraw = do
  eventControllerKey <- Gtk.eventControllerKeyNew
  _ <- Gtk.onEventControllerKeyKeyPressed eventControllerKey \keyval _keycode _mods -> do
    case keyval of
      Gdk.KEY_space -> do
        updated <- atomicModifyIORef' appStateRef appStateStep
        when updated queueRedraw
        return updated
      _ -> return False
  return eventControllerKey

renderBackground :: (?style :: Style) => Double -> Double -> Cairo.Render ()
renderBackground w h = do
  Cairo.rectangle 0 0 w h
  setColor ?style.backgroundColor
  Cairo.fill