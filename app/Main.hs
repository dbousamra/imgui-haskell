{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

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
  asSlider :: Int
}

draw :: AppState -> IO AppState
draw state = do
  ImGui.showMetricsWindow True
  ImGui.begin "Window" 0 0
  sliderValue <- ImGui.sliderInt "Slider1" (asSlider state)  1 100 "Stuff"
  sliderValue2 <- ImGui.sliderInt2 "Slider2" [10, 20]  1 100 "Stuff"

  let newState = state { asSlider =  sliderValue }

  ImGui.end
  pure newState

main :: IO ()
main = runImGuiApp $
  ImGuiApp {
    appDraw = draw,
    appState = AppState {
      asSlider = 50
    }
  }
