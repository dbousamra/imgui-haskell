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
  asSliderFloat :: Float,
  asSliderAngle :: Float,
  asColorPicker :: [Float]
}

draw :: AppState -> IO AppState
draw state = do
  ImGui.showMetricsWindow True

  style <- ImGui.getStyle
  ImGui.showStyleEditor style


  ImGui.begin "Window" 0 0
  sliderIntV <- ImGui.sliderInt "Slider int" (asSliderInt state)  1 100 ""
  sliderFloatV <- ImGui.sliderFloat "Slider float" (asSliderFloat state)  1.0 100.0 "" 1.0
  sliderAngleV <- ImGui.sliderAngle "Slider angle" (asSliderAngle state)  0.0 360.0 ""
  colorPickerV <- ImGui.colorPicker3 "Color picker" (asColorPicker state) ImGui.ImGuiColorEditFlags_None

  let newState = state {
    asSliderInt =  sliderIntV,
    asSliderFloat = sliderFloatV,
    asSliderAngle = sliderAngleV,
    asColorPicker = colorPickerV
  }


  ImGui.end
  pure newState

main :: IO ()
main = runImGuiApp $
  ImGuiApp {
    appDraw = draw,
    appState = AppState {
      asSliderInt = 50,
      asSliderFloat = 50.0,
      asSliderAngle = pi,
      asColorPicker = [0.0, 0.0, 0.0]
    }
  }
