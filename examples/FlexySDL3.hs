{-# LANGUAGE ForeignFunctionInterface #-}

-- Example: Flexy + SDL 3.4 (minimal FFI bindings)
--
-- Build/run (after adding the executable stanza in flexy.cabal):
--   cabal run flexy-sdl3-example
--
-- Controls:
--   D - toggle sidebar visibility (DisplayNone)
--   Esc / window close - quit

module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool(..), CFloat(..), CInt(..))

import Flexy

-- Opaque SDL types

data SDL_Window

data SDL_Renderer

-- SDL_FRect (float rectangle)
data SDL_FRect = SDL_FRect CFloat CFloat CFloat CFloat

instance Storable SDL_FRect where
  sizeOf _ = 4 * sizeOf (undefined :: CFloat)
  alignment _ = alignment (undefined :: CFloat)
  peek ptr =
    SDL_FRect
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr (sizeOf (undefined :: CFloat))
      <*> peekByteOff ptr (2 * sizeOf (undefined :: CFloat))
      <*> peekByteOff ptr (3 * sizeOf (undefined :: CFloat))
  poke ptr (SDL_FRect x y w h) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr (sizeOf (undefined :: CFloat)) y
    pokeByteOff ptr (2 * sizeOf (undefined :: CFloat)) w
    pokeByteOff ptr (3 * sizeOf (undefined :: CFloat)) h

-- Minimal SDL3 bindings

foreign import ccall "SDL_Init" sdlInit :: Word32 -> IO CInt
foreign import ccall "SDL_Quit" sdlQuit :: IO ()

foreign import ccall "SDL_CreateWindow" sdlCreateWindow :: CString -> CInt -> CInt -> Word32 -> IO (Ptr SDL_Window)
foreign import ccall "SDL_DestroyWindow" sdlDestroyWindow :: Ptr SDL_Window -> IO ()

foreign import ccall "SDL_CreateRenderer" sdlCreateRenderer :: Ptr SDL_Window -> CString -> IO (Ptr SDL_Renderer)
foreign import ccall "SDL_DestroyRenderer" sdlDestroyRenderer :: Ptr SDL_Renderer -> IO ()

foreign import ccall "SDL_SetRenderDrawColor" sdlSetRenderDrawColor :: Ptr SDL_Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool
foreign import ccall "SDL_RenderClear" sdlRenderClear :: Ptr SDL_Renderer -> IO CBool
foreign import ccall "SDL_RenderFillRect" sdlRenderFillRect :: Ptr SDL_Renderer -> Ptr SDL_FRect -> IO CBool
foreign import ccall "SDL_RenderPresent" sdlRenderPresent :: Ptr SDL_Renderer -> IO CBool

foreign import ccall "SDL_Delay" sdlDelay :: Word32 -> IO ()

foreign import ccall "flexy_sdl_init_video_flag" sdlInitVideoFlag :: IO Word32
foreign import ccall "flexy_sdl_poll_event" sdlPollEvent :: Ptr CInt -> IO CInt

main :: IO ()
main = do
  flags <- sdlInitVideoFlag
  rc <- sdlInit flags
  when (rc /= 0) (fail "SDL_Init failed")

  let winW = 900
      winH = 600

  window <- withCString "Flexy + SDL3" $ \title -> sdlCreateWindow title winW winH 0
  when (window == nullPtr) (fail "SDL_CreateWindow failed")

  renderer <- sdlCreateRenderer window nullPtr
  when (renderer == nullPtr) (fail "SDL_CreateRenderer failed")

  sidebarVisible <- newIORef True
  loop renderer winW winH sidebarVisible

  sdlDestroyRenderer renderer
  sdlDestroyWindow window
  sdlQuit

loop :: Ptr SDL_Renderer -> CInt -> CInt -> IORef Bool -> IO ()
loop renderer winW winH sidebarVisible = do
  (quit, toggled) <- pollEvents
  when toggled (modifyIORef' sidebarVisible not)

  showSidebar <- readIORef sidebarVisible
  let root = buildLayout showSidebar (fromIntegral winW) (fromIntegral winH)
      layoutRoot = computeLayout defaultConfig (Size (DimPoints (fromIntegral winW)) (DimPoints (fromIntegral winH))) root

  _ <- sdlSetRenderDrawColor renderer 20 20 25 255
  _ <- sdlRenderClear renderer

  drawLayout renderer 0 layoutRoot

  _ <- sdlRenderPresent renderer

  sdlDelay 16
  unless quit (loop renderer winW winH sidebarVisible)

pollEvents :: IO (Bool, Bool)
pollEvents = alloca $ \codePtr ->
  let go quit toggled = do
        hasEvent <- sdlPollEvent codePtr
        if hasEvent == 0
          then pure (quit, toggled)
          else do
            code <- peek codePtr
            let quit' = quit || code == 1
                toggled' = toggled || code == 2
            go quit' toggled'
  in go False False

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
drawLayout :: Ptr SDL_Renderer -> Int -> LayoutNode -> IO ()
drawLayout renderer depth layoutNode = do
  let (x, y, w, h) = layoutBounds layoutNode
      rect = SDL_FRect (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)
      (r, g, b, a) = colorFor depth (fromMaybe "" (layoutKey layoutNode))
  _ <- sdlSetRenderDrawColor renderer r g b a
  with rect $ \rectPtr -> do
    _ <- sdlRenderFillRect renderer rectPtr
    pure ()
  forM_ (layoutChildren layoutNode) (drawLayout renderer (depth + 1))

colorFor :: Int -> String -> (Word8, Word8, Word8, Word8)
colorFor depth key =
  case key of
    "root" -> (30, 30, 36, 255)
    "header" -> (70, 120, 180, 255)
    "body" -> (45, 45, 50, 255)
    "sidebar" -> (160, 90, 60, 255)
    "content" -> (70, 160, 120, 255)
    _ ->
      case depth `mod` 4 of
        0 -> (90, 90, 100, 255)
        1 -> (120, 90, 130, 255)
        2 -> (90, 130, 120, 255)
        _ -> (130, 110, 90, 255)
