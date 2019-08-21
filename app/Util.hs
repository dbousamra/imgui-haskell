{-# LANGUAGE OverloadedStrings #-}

module Util (
    runImGuiApp
  , ImGuiApp(..)
) where

import           Control.Monad             (unless)
import qualified Graphics.Rendering.OpenGL as GL
import qualified ImGui                     as ImGui
import           SDL                       (($=))
import qualified SDL                       as SDL
import qualified SDL.Internal.Types        as SDL
import qualified SDL.Raw                   as SDLRaw
import           SDL.Vect
import           Foreign                   (alloca)


data ImGuiApp s = ImGuiApp {
    appDraw :: s -> IO s
  , appState :: s
}


runImGuiApp :: ImGuiApp s -> IO ()
runImGuiApp app = do
  SDL.initialize [SDL.InitVideo]

  window @ (SDL.Window wp) <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {
        SDL.windowHighDPI = True,
        SDL.windowInitialSize = V2 640 480
      }
  SDL.showWindow window
  glContext <- SDLRaw.glCreateContext wp
  ImGui.gl3wInit
  imguiContext <- ImGui.createContext Nothing
  ImGui.getIO
  ImGui.initForOpenGL wp glContext
  ImGui.openGL3Init "#version 110"

  loop window app

  ImGui.destroyContext imguiContext
  SDL.destroyWindow window
  SDL.quit


loop :: SDL.Window -> ImGuiApp s -> IO ()
loop window @ (SDL.Window wp) app = do

  alloca (\e -> do
    SDLRaw.pollEvent e
    ImGui.sdl2ProcessEvent e)
  -- event <- SDL.pollEvent
  -- quit <- case event of
  --   Just e -> do
  --     ImGui.sdl2ProcessEvent e
  --     pure $ SDL.eventPayload e == SDL.QuitEvent
  --   Nothing -> 
  --     pure False

  let quit = False

  ImGui.openGL3NewFrame
  ImGui.sdl2NewFrame wp
  ImGui.newFrame

  let state = appState app
  let draw = appDraw app
  newState <- draw state
  let newApp = ImGuiApp {
    appDraw = draw,
    appState = newState
  }

  ImGui.render

  GL.clear [GL.ColorBuffer]
  ImGui.getDrawData >>= ImGui.openGL3RenderDrawData
  SDL.glSwapWindow window

  
  unless quit (loop window newApp)
