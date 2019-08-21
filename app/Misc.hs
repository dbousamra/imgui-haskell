{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Misc (main) where

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
  asColorPicker :: [Float],
  asButton      :: Bool
} deriving (Eq, Show)

draw :: AppState -> IO AppState
draw state = do
  ImGui.showMetricsWindow True


  ImGui.begin "Window" False ImGui.ImGuiWindowFlags_None
  (_, sliderIntV) <- ImGui.sliderInt "Slider int" (asSliderInt state)  1 100 ""
  (_, sliderFloatV) <- ImGui.sliderFloat "Slider float" (asSliderFloat state)  1.0 100.0 "" 1.0
  (_, sliderAngleV) <- ImGui.sliderAngle "Slider angle" (asSliderAngle state)  0.0 360.0 ""
  (_, colorPickerV) <- ImGui.colorPicker3 "Color picker" (asColorPicker state) ImGui.ImGuiColorEditFlags_None
  (buttonV) <- ImGui.button "Press me" (ImGui.makeImVec2 0 0)
  ImGui.text $ "Button state = " ++ show buttonV


  let newState = state {
    asSliderInt =  sliderIntV,
    asSliderFloat = sliderFloatV,
    asSliderAngle = sliderAngleV,
    asColorPicker = colorPickerV,
    asButton = buttonV
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
      asColorPicker = [0.0, 0.0, 0.0],
      asButton = False
    }
  }
