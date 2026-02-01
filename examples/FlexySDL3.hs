{-# LANGUAGE OverloadedStrings #-}

-- Example: Flexy + SDL 3.4
--
-- This is a small demo that computes a flex layout with Flexy and renders
-- the resulting rectangles with SDL. It targets the Haskell SDL3 bindings,
-- but the exact API names may vary slightly between bindings.
--
-- Controls:
--   D - toggle sidebar visibility (DisplayNone)
--   Esc / window close - quit

module Main (main) where

import Control.Monad (forM_, unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt)
import Data.Word (Word8)

import Flexy

-- NOTE: This assumes SDL3 bindings that expose an SDL module similar to sdl2.
-- Adjust imports and names if your bindings differ.
import qualified SDL

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  let winSize = SDL.V2 900 600
  window <- SDL.createWindow "Flexy + SDL3" SDL.defaultWindow
    { SDL.windowInitialSize = winSize
    }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  sidebarVisible <- newIORef True
  loop renderer winSize sidebarVisible

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

loop :: SDL.Renderer -> SDL.V2 CInt -> IORef Bool -> IO ()
loop renderer (SDL.V2 winW winH) sidebarVisible = do
  events <- SDL.pollEvents
  let quit = any isQuit events
  forM_ events (handleToggle sidebarVisible)

  showSidebar <- readIORef sidebarVisible
  let root = buildLayout showSidebar (fromIntegral winW) (fromIntegral winH)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints (fromIntegral winW)) (DimPoints (fromIntegral winH))) root

  SDL.rendererDrawColor renderer SDL.$= SDL.V4 20 20 25 255
  SDL.clear renderer

  drawLayout renderer 0 layoutRoot

  SDL.present renderer
  unless quit (loop renderer (SDL.V2 winW winH) sidebarVisible)

isQuit :: SDL.Event -> Bool
isQuit e =
  case SDL.eventPayload e of
    SDL.QuitEvent -> True
    SDL.KeyboardEvent ke ->
      SDL.keyboardEventKeyMotion ke == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeEscape
    _ -> False

handleToggle :: IORef Bool -> SDL.Event -> IO ()
handleToggle ref e =
  case SDL.eventPayload e of
    SDL.KeyboardEvent ke ->
      if SDL.keyboardEventKeyMotion ke == SDL.Pressed
          && SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeD
        then modifyIORef' ref not
        else pure ()
    _ -> pure ()

buildLayout :: Bool -> Float -> Float -> Node
buildLayout showSidebar winW winH =
  let headerStyle = defaultStyle
        & setHeight (DimPoints 60)
        & setAlignItems AlignCenter
      header = withKey "header" (node headerStyle)

      sidebarStyle = defaultStyle
        & setWidth (DimPoints 220)
        & setDisplay (if showSidebar then DisplayFlex else DisplayNone)
      sidebar = withKey "sidebar" (node sidebarStyle)

      contentStyle = defaultStyle
        & setFlexGrow 1
        & setGaps 12 12
      content = withKey "content" (node contentStyle)

      bodyStyle = defaultStyle
        & setFlexDirection Row
        & setFlexGrow 1
        & setGaps 12 12
      body = withKey "body" (withChildren [sidebar, content] (node bodyStyle))

      rootStyle = defaultStyle
        & setFlexDirection Column
        & setWidth (DimPoints winW)
        & setHeight (DimPoints winH)
        & setGaps 12 12
        & setPaddingAll (ValPoints 16)
      root = withKey "root" (node rootStyle)
  in withChildren [header, body] root

-- Draw each layout node as a filled rectangle.
-- Color is picked by key/depth for easy visual scanning.
drawLayout :: SDL.Renderer -> Int -> LayoutNode -> IO ()
drawLayout renderer depth node = do
  let (x, y, w, h) = layoutBounds node
      rect = SDL.Rectangle (SDL.P (SDL.V2 (roundToInt x) (roundToInt y))) (SDL.V2 (roundToInt w) (roundToInt h))
      color = colorFor depth (fromMaybe "" (layoutKey node))
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just rect)

  forM_ (layoutChildren node) (drawLayout renderer (depth + 1))

colorFor :: Int -> String -> SDL.V4 Word8
colorFor depth key =
  case key of
    "root" -> SDL.V4 30 30 36 255
    "header" -> SDL.V4 70 120 180 255
    "body" -> SDL.V4 45 45 50 255
    "sidebar" -> SDL.V4 160 90 60 255
    "content" -> SDL.V4 70 160 120 255
    _ ->
      case depth `mod` 4 of
        0 -> SDL.V4 90 90 100 255
        1 -> SDL.V4 120 90 130 255
        2 -> SDL.V4 90 130 120 255
        _ -> SDL.V4 130 110 90 255

roundToInt :: Float -> CInt
roundToInt = fromIntegral . max 0 . round
