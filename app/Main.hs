{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad             (unless)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Lib                       as Imgui
import           SDL                       (($=))
import qualified SDL                       as SDL
import qualified SDL.Internal.Types        as SDL
import qualified SDL.Raw                   as SDLRaw
import           SDL.Vect

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window @ (SDL.Window wp) <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {
        SDL.windowInitialSize = V2 1024 768
      }
  SDL.showWindow window
  glContext <- SDLRaw.glCreateContext wp
  Imgui.gl3wInit
  Imgui.createContext Nothing
  Imgui.getIO
  Imgui.initForOpenGL wp glContext
  Imgui.openGL3Init "#version 110"

  loop window 0.0

  SDL.destroyWindow window
  SDL.quit


loop :: SDL.Window -> Float -> IO ()
loop window @ (SDL.Window wp) time = do
  events <- SDL.pollEvents

  Imgui.openGL3NewFrame
  Imgui.sdl2NewFrame wp
  Imgui.newFrame

  let dim = pulse time

  Imgui.begin "Window" 0 0
  Imgui.text "Hello from imgui"
  Imgui.button "Close me" $ Imgui.makeImVec2 dim dim
  Imgui.end

  Imgui.render

  GL.clear [GL.ColorBuffer]
  Imgui.getDrawData >>= Imgui.openGL3RenderDrawData
  SDL.glSwapWindow window

  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  unless quit (loop window (time + 1.0))


pulse :: Float -> Float
pulse time = amplitude * 0.5 * (1 + (sin (2 * pi * frequency * time)))
  where
    pi = 3.14
    frequency = 10
    amplitude = 100

  
