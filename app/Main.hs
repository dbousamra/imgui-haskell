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
  asSliderInt   :: Int,
  asSliderFloat :: Float
}

draw :: AppState -> IO AppState
draw state = do
  ImGui.showMetricsWindow True
  ImGui.begin "Window" 0 0
  sliderIntV <- ImGui.sliderInt "Slider int" (asSliderInt state)  1 100 ""
  sliderFloatV <- ImGui.sliderFloat "Slider float" (asSliderFloat state)  1.0 100.0 "" 1.0

  let newState = state {
    asSliderInt =  sliderIntV,
    asSliderFloat = sliderFloatV
  }

  ImGui.end
  pure newState

main :: IO ()
main = runImGuiApp $
  ImGuiApp {
    appDraw = draw,
    appState = AppState {
      asSliderInt = 50,
      asSliderFloat = 50.0
    }
  }
