{-# LANGUAGE ImplicitParams #-}

module Stepper.Interactive (runInteractiveApp) where

import Prelude hiding (mod)

import Data.IText
import Data.Text (Text)
import qualified Data.Text as Text

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Pango as Pango
import Data.IORef
import System.IO.Unsafe
import Control.Monad

import Stepper.Syntax.Scoped
import Stepper.Render
import Stepper.Render.Style
import Stepper.Render.Layout
import Stepper.Evaluator

data Stack a = Bottom a | Push a (Stack a)

pop :: Stack a -> Maybe (Stack a)
pop (Bottom _) = Nothing
pop (Push _ stk) = Just stk

peek :: Stack a -> a
peek (Bottom x) = x
peek (Push x _) = x

data Stats = MkStats { reductions :: Int, gcs :: Int }

succReductions :: Stats -> Stats
succReductions stats = stats { reductions = stats.reductions + 1 }

succGCs :: Stats -> Stats
succGCs stats = stats { gcs = stats.gcs + 1 }

data AppState =
  MkAppState {
    steps :: Stack (Stats, Module),
    entryPoint :: IText,
    lastLayout :: Layout
  }

appStateStep :: AppState -> (AppState, Bool)
appStateStep appState =
  let (stats, mod) = peek appState.steps in
  case evalstep mod (TopIdUser appState.entryPoint) of
    Nothing -> (appState, False)
    Just mod' ->
      let stats' = succReductions stats in
      (appState{ steps = Push (stats', mod') appState.steps }, True)

appStateGC :: AppState -> (AppState, Bool)
appStateGC appState =
  let (stats, mod) = peek appState.steps in
  case gc mod (TopIdUser appState.entryPoint) of
    Nothing -> (appState, False)
    Just mod' ->
      let stats' = succGCs stats in
      (appState{ steps = Push (stats', mod') appState.steps }, True)

appStateUndo :: AppState -> (AppState, Bool)
appStateUndo appState =
  case pop appState.steps of
    Nothing -> (appState, False)
    Just steps' -> (appState { steps = steps' }, True)

appUpdateLayout :: (?style :: Style) => Pango.Context -> IORef FontCache -> Extents -> AppState -> (AppState, Layout)
appUpdateLayout pangoContext fontCacheRef extents appState = (appState { lastLayout = layout }, layout)
  where
    layout :: Layout
    layout =
      let mkTextLayout :: Text -> Int -> Text -> Color -> Layout
          mkTextLayout fontFamily fontSize str = unsafePerformIO do
            createTextLayout pangoContext fontCacheRef fontFamily fontSize str
      in
        withLayoutCtx LCtx{style = ?style, mkTextLayout} $
          let (stats, mod) = peek appState.steps in
          renderStats stats.reductions stats.gcs `vert`
          renderModule extents mod

runInteractiveApp :: Module -> IText -> IO ()
runInteractiveApp srcMod entryPoint = do
  appStateRef <- newIORef $
    MkAppState {
      steps = Bottom (MkStats 0 0, srcMod),
      entryPoint,
      lastLayout = L 0 0 (const (return ()))
    }
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
        identColor      = RGB 0.88 0.88 0.88,
        localIdentColor = RGB 0.11 0.82 0.75,
        punctColor      = RGB 0.60 0.60 0.60,
        borderColor     = RGB 0.44 0.44 0.44,
        borderWidth     = 2
      }

  drawingArea <- createDrawingArea (readIORef appStateRef)
  Gtk.drawingAreaSetContentHeight drawingArea 1000

  scrolledWindow <- Gtk.scrolledWindowNew
  Gtk.scrolledWindowSetChild scrolledWindow (Just drawingArea)

  window <- Gtk.applicationWindowNew app
  Gtk.setWindowDefaultWidth window 800
  Gtk.setWindowDefaultHeight window 600
  Gtk.windowSetChild window (Just scrolledWindow)

  fontCacheRef <- newIORef emptyFontCache
  let updateLayoutM :: Extents -> IO ()
      updateLayoutM extents = do
        pangoContext <- Gtk.widgetGetPangoContext drawingArea
        layout <- atomicModifyIORef' appStateRef (appUpdateLayout pangoContext fontCacheRef extents)
        Gtk.drawingAreaSetContentWidth drawingArea (fromIntegral layout.extents.w)
        Gtk.drawingAreaSetContentHeight drawingArea (fromIntegral layout.extents.h)
        Gtk.widgetQueueDraw drawingArea

  Gtk.onDrawingAreaResize drawingArea $ \w h ->
    updateLayoutM E{w = fromIntegral w, h = fromIntegral h}

  eventControllerKey <- createEventControllerKey appStateRef $ do
    w <- Gtk.drawingAreaGetContentWidth drawingArea
    h <- Gtk.drawingAreaGetContentHeight drawingArea
    updateLayoutM E{w = fromIntegral w, h = fromIntegral h}
  Gtk.widgetAddController window eventControllerKey

  Gtk.windowPresent window

createDrawingArea :: (?style :: Style) => IO AppState -> IO Gtk.DrawingArea
createDrawingArea readAppState = do
  drawingArea <- Gtk.drawingAreaNew
  Gtk.drawingAreaSetDrawFunc drawingArea $
    Just $ \_ ctx _ _ ->
    flip Cairo.renderWithContext ctx $ do
      appState <- Cairo.liftIO readAppState
      (x1, y1, x2, y2) <- Cairo.clipExtents
      let (w, h) = (x2 - x1, y2 - y1)
      renderBackground w h
      appState.lastLayout.render 0
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
      Gdk.KEY_g -> do
        updated <- atomicModifyIORef' appStateRef appStateGC
        when updated queueRedraw
        return updated
      Gdk.KEY_u -> do
        updated <- atomicModifyIORef' appStateRef appStateUndo
        when updated queueRedraw
        return updated
      _ -> return False
  return eventControllerKey

renderBackground :: (?style :: Style) => Double -> Double -> Cairo.Render ()
renderBackground w h = do
  Cairo.rectangle 0 0 w h
  setColor ?style.backgroundColor
  Cairo.fill