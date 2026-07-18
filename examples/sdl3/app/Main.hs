{-# LANGUAGE ForeignFunctionInterface #-}

-- Example: Flexy + SDL 3.4 (minimal FFI bindings)
--
-- Build/run:
--   cd examples
--   cabal run flexy-sdl3-example
--
-- Controls:
--   D - toggle sidebar visibility
--   Esc / window close - quit

module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Foreign
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool(..), CFloat(..), CInt(..))
import System.Environment (lookupEnv)

import Flexy

-- Opaque SDL types

data SDLWindow

data SDLRenderer

-- SDL_FRect (float rectangle)
data SDLFRect = SDLFRect CFloat CFloat CFloat CFloat

instance Storable SDLFRect where
  sizeOf _ = 4 * sizeOf (undefined :: CFloat)
  alignment _ = alignment (undefined :: CFloat)
  peek ptr =
    SDLFRect
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr (sizeOf (undefined :: CFloat))
      <*> peekByteOff ptr (2 * sizeOf (undefined :: CFloat))
      <*> peekByteOff ptr (3 * sizeOf (undefined :: CFloat))
  poke ptr (SDLFRect x y w h) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr (sizeOf (undefined :: CFloat)) y
    pokeByteOff ptr (2 * sizeOf (undefined :: CFloat)) w
    pokeByteOff ptr (3 * sizeOf (undefined :: CFloat)) h

-- Minimal SDL3 bindings

foreign import ccall "SDL_Init" sdlInit :: Word32 -> IO CBool
foreign import ccall "SDL_Quit" sdlQuit :: IO ()

foreign import ccall "SDL_CreateWindow" sdlCreateWindow :: CString -> CInt -> CInt -> Word32 -> IO (Ptr SDLWindow)
foreign import ccall "SDL_DestroyWindow" sdlDestroyWindow :: Ptr SDLWindow -> IO ()

foreign import ccall "SDL_CreateRenderer" sdlCreateRenderer :: Ptr SDLWindow -> CString -> IO (Ptr SDLRenderer)
foreign import ccall "SDL_DestroyRenderer" sdlDestroyRenderer :: Ptr SDLRenderer -> IO ()

foreign import ccall "SDL_SetRenderDrawColor" sdlSetRenderDrawColor :: Ptr SDLRenderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool
foreign import ccall "SDL_RenderClear" sdlRenderClear :: Ptr SDLRenderer -> IO CBool
foreign import ccall "SDL_RenderFillRect" sdlRenderFillRect :: Ptr SDLRenderer -> Ptr SDLFRect -> IO CBool
foreign import ccall "SDL_RenderPresent" sdlRenderPresent :: Ptr SDLRenderer -> IO CBool

foreign import ccall "SDL_Delay" sdlDelay :: Word32 -> IO ()
foreign import ccall "SDL_GetError" sdlGetError :: IO CString
foreign import ccall "SDL_SetHint" sdlSetHint :: CString -> CString -> IO CBool
foreign import ccall "SDL_GetNumVideoDrivers" sdlGetNumVideoDrivers :: IO CInt
foreign import ccall "SDL_GetVideoDriver" sdlGetVideoDriver :: CInt -> IO CString

foreign import ccall "flexy_sdl_init_video_flag" sdlInitVideoFlag :: IO Word32
foreign import ccall "flexy_sdl_poll_event" sdlPollEvent :: Ptr CInt -> IO CInt

main :: IO ()
main = do
  flags <- sdlInitVideoFlag
  initSDL flags

  let winW = 900
      winH = 600

  window <- withCString "Flexy + SDL3" $ \title -> sdlCreateWindow title winW winH 0
  when (window == nullPtr) (sdlFail "SDL_CreateWindow")

  renderer <- sdlCreateRenderer window nullPtr
  when (renderer == nullPtr) (sdlFail "SDL_CreateRenderer")

  sidebarVisible <- newIORef True
  loop renderer winW winH sidebarVisible

  sdlDestroyRenderer renderer
  sdlDestroyWindow window
  sdlQuit

sdlFail :: String -> IO a
sdlFail label = do
  err <- sdlErrorString
  fail (label <> " failed: " <> formatError err)

sdlErrorString :: IO String
sdlErrorString = do
  errPtr <- sdlGetError
  if errPtr == nullPtr then pure "" else peekCString errPtr

formatError :: String -> String
formatError err =
  if null err
    then "<no error>"
    else err

initSDL :: Word32 -> IO ()
initSDL flags = do
  initialized <- sdlInit flags
  if initialized /= 0
    then pure ()
    else do
      err <- sdlErrorString
      drivers <- getVideoDrivers
      envDriver <- lookupEnv "SDL_VIDEO_DRIVER"
      envDriverCompat <- lookupEnv "SDL_VIDEODRIVER"
      if isNothing envDriver && isNothing envDriverCompat && "dummy" `elem` drivers
        then do
          putStrLn "SDL_Init failed; falling back to dummy video driver."
          _ <- setVideoDriverHint "dummy"
          sdlQuit
          initializedWithDummy <- sdlInit flags
          when (initializedWithDummy == 0) (sdlFailDetailed "SDL_Init" err drivers)
        else sdlFailDetailed "SDL_Init" err drivers

setVideoDriverHint :: String -> IO ()
setVideoDriverHint driver =
  withCString "SDL_VIDEO_DRIVER" $ \name ->
    withCString driver $ \value -> do
      _ <- sdlSetHint name value
      pure ()

getVideoDrivers :: IO [String]
getVideoDrivers = do
  count <- sdlGetNumVideoDrivers
  let n = fromIntegral (max 0 count) :: Int
  forM [0 .. n - 1] $ \idx -> do
    namePtr <- sdlGetVideoDriver (fromIntegral idx)
    if namePtr == nullPtr then pure "" else peekCString namePtr

sdlFailDetailed :: String -> String -> [String] -> IO a
sdlFailDetailed label err drivers =
  fail (label <> " failed: " <> formatError err <> driverHint drivers)

driverHint :: [String] -> String
driverHint drivers =
  case filter (not . null) drivers of
    [] -> ""
    names ->
      "\nAvailable video drivers: "
        <> intercalate ", " names
        <> "\nHint: set SDL_VIDEO_DRIVER=<driver>"

loop :: Ptr SDLRenderer -> CInt -> CInt -> IORef Bool -> IO ()
loop renderer winW winH sidebarVisible = do
  (quit, toggled) <- pollEvents
  when toggled (modifyIORef' sidebarVisible not)

  showSidebar <- readIORef sidebarVisible
  let root = buildLayout showSidebar
      layoutRoot = layout (Size (fromIntegral winW) (fromIntegral winH)) root

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

buildLayout :: Bool -> Node String
buildLayout showSidebar =
  styled (padding (allEdges 16) <> gap 12) $
    column "root"
      [ styled (height (Points 60) <> align AlignCenter) (leaf "header")
      , styled (grow 1 <> gap 12) (row "body" bodyChildren)
      ]
  where
    sidebar = styled (width (Points 220)) (leaf "sidebar")
    content = styled (grow 1) (leaf "content")
    bodyChildren
      | showSidebar = [sidebar, content]
      | otherwise = [content]

-- Draw each layout node as a filled rectangle.
drawLayout :: Ptr SDLRenderer -> Int -> Layout String -> IO ()
drawLayout renderer depth layoutNode = do
  let Rect x y w h = bounds layoutNode
      rect = SDLFRect (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)
      (r, g, b, a) = colorFor depth (value layoutNode)
  _ <- sdlSetRenderDrawColor renderer r g b a
  with rect $ \rectPtr -> do
    _ <- sdlRenderFillRect renderer rectPtr
    pure ()
  forM_ (children layoutNode) (drawLayout renderer (depth + 1))

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
