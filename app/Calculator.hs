{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Calculator (main) where

import           Foreign.Ptr               (nullFunPtr, nullPtr)
import           Control.Monad             (unless)
import qualified Graphics.Rendering.OpenGL as GL
import qualified ImGui                     as ImGui
import           SDL                       (($=))
import qualified SDL                       as SDL
import qualified SDL.Internal.Types        as SDL
import qualified SDL.Raw                   as SDLRaw
import           SDL.Vect
import           Util

data AppState = AppState {
  asBgColor     :: [Float],
  asWindowTitle :: String
} deriving (Eq, Show)


draw :: AppState -> IO AppState
draw state = do
  ImGui.begin "Window" False ImGui.ImGuiWindowFlags_None
  (success, bgColor @ [r, g, b]) <- ImGui.colorEdit3 "Background color" (asBgColor state) ImGui.ImGuiColorEditFlags_None
  (success, windowTitle) <- ImGui.inputText "Edit window title" (asWindowTitle state) 64 ImGui.ImGuiInputTextFlags_None nullFunPtr nullPtr

  let newState = state { 
    asBgColor = bgColor,
    asWindowTitle = windowTitle
  }
  ImGui.end
  GL.clearColor $= GL.Color4 r g b 1
  pure newState

main :: IO ()
main = runImGuiApp $
  ImGuiApp {
    appDraw = draw,
    appState = AppState {
      asBgColor = [0.0, 0.0, 0.0],
      asWindowTitle = "Hello world!"
    }
  }
