{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include <cimgui.h>
#include <cimgui_impl.h>
#include <GL/gl3w.h>

{# context lib="cimgui" #}

module ImGui.Internal.Bindings where


import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import qualified SDL.Raw.Types           as SDL
import qualified SDL.Internal.Types           as SDL hiding (Window)


{#pointer *ImVec2 as ImVec2Ptr foreign newtype#}
{#pointer *ImVec4 as ImVec4Ptr foreign newtype#}
{#pointer *ImFontAtlas as ImFontAtlas  #}
{#pointer *ImDrawData as ImDrawData  #}
{#pointer *ImGuiContext as ImGuiContext  #}
{#pointer *ImGuiIO as ImGuiIO #}
{#pointer *ImGuiStyle as ImGuiStyle #}
{#pointer *ImGuiWindowFlags as ImGuiWindowFlags  #}

-- Imgui functions
{# fun igCreateContext as createContext_ { `ImFontAtlas' } -> `ImGuiContext' #}
{# fun igDestroyContext as destroyContext { `ImGuiContext' } -> `()' #}
{# fun igGetCurrentContext as getCurrentContext { } -> `ImGuiContext' #}
{# fun igSetCurrentContext as setCurrentContext { `ImGuiContext' } -> `()' #}
{# fun igGetIO as getIO { } -> `ImGuiIO' #}
{# fun igGetStyle as getStyle { } -> `ImGuiStyle' #}
{# fun igNewFrame as newFrame { } -> `()' #}
{# fun igEndFrame as endFrame { } -> `()' #}
{# fun igRender as render { } -> `()' #}
{# fun igGetDrawData as getDrawData { } -> `ImDrawData' #}
{# fun igShowDemoWindow as showDemoWindow { `Bool' } -> `()' #}
{# fun igShowAboutWindow as showAboutWindow { `Bool' } -> `()' #}
{# fun igShowMetricsWindow as showMetricsWindow { `Bool' } -> `()' #}
{# fun igShowStyleEditor as showStyleEditor { `ImGuiStyle' } -> `()' #}
{# fun igShowStyleSelector as showStyleSelector { `String' } -> `Bool' #}
{# fun igShowFontSelector as showFontSelector { `String' } -> `()' #}
{# fun igShowUserGuide as showUserGuide { } -> `()' #}
{# fun igGetVersion as getVersion { } -> `String' #}

{# fun igBegin as begin { `String', `Int', `Int' } -> `()' #}
{# fun igEnd as end { } -> `()' #}
{# fun igButton as button {`String', %`ImVec2Ptr'} -> `Bool'#}
{# fun igText as text { `String' } -> `()' #}




{# fun igPushStyleColor as pushStyleColor { cFromEnum `ImGuiCol', %`ImVec4Ptr' } -> `()' #}
{# fun igPopStyleColor as popStyleColor { `Int' } -> `()' #}


{#fun unsafe igSliderFloat as sliderFloat
  { `String'
  , `Float' peekReal*
  , `Float'
  , `Float'
  , `String'
  , `Float'
  } -> `()' #}

{#fun unsafe igSliderFloat2 as sliderFloat2
  { `String'
  , withArrayConvReal * `[Float]' peekRealArray2*
  , `Float'
  , `Float'
  , `String'
  , `Float'
  } -> `()' #}

{#fun unsafe igSliderFloat3 as sliderFloat3
  { `String'
  , withArrayConvReal * `[Float]' peekRealArray3*
  , `Float'
  , `Float'
  , `String'
  , `Float'
  } -> `()' #}

{#fun unsafe igSliderFloat4 as sliderFloat4
  { `String'
  , withArrayConvReal * `[Float]' peekRealArray4*
  , `Float'
  , `Float'
  , `String'
  , `Float'
  } -> `()' #}

{#fun unsafe igSliderAngle as sliderAngle
  { `String'
  , `Float' peekReal*
  , `Float'
  , `Float'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt as sliderInt
  { `String'
  , `Int' peekIntegral*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt2 as sliderInt2
  { `String'
  , withArrayConvIntegral * `[Int]' peekIntegralArray2*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt3 as sliderInt3
  { `String'
  , withArrayConvIntegral * `[Int]' peekIntegralArray3*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt4 as sliderInt4
  { `String'
  , withArrayConvIntegral * `[Int]' peekIntegralArray4*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igColorPicker3 as colorPicker3
  { `String'
  , withArrayConvReal * `[Float]' peekRealArray3*
  , cFromEnum `ImGuiColorEditFlags'
  } -> `()' #}

{#fun unsafe igColorPicker4 as colorPicker4
  { `String'
  , withArrayConvReal * `[Float]' peekRealArray4*
  , cFromEnum `ImGuiColorEditFlags'
  , `Float'
  } -> `()' #}

-- Creating structs
{# fun pure ImVec2_ImVec2Float as makeImVec2 {`Float', `Float'} -> `ImVec2Ptr'#}
{# fun pure ImVec4_ImVec4Float as makeImVec4 {`Float', `Float', `Float', `Float'} -> `ImVec4Ptr'#}

-- ImGui Impl functions
{# fun gl3wInit as gl3wInit { } -> `()' #}
{# fun ImGui_ImplOpenGL3_Init as openGL3Init { `String' } -> `Bool' #}
{# fun ImGui_ImplOpenGL3_NewFrame as openGL3NewFrame { } -> `()' #}
{# fun ImGui_ImplOpenGL3_RenderDrawData as openGL3RenderDrawData { `ImDrawData' } -> `()' #}
{# fun ImGui_ImplSDL2_InitForOpenGL as initForOpenGL { castPtr `SDL.Window', castPtr `SDL.GLContext' } -> `Bool' #}
{# fun ImGui_ImplSDL2_NewFrame as sdl2NewFrame { castPtr `SDL.Window' } -> `()' #}
{# fun ImGui_ImplSDL2_Shutdown as shutdown { } -> `()' #}

-- Enums
{# enum ImGuiCol_ as ImGuiCol {} deriving (Show, Eq) #}
{# enum ImGuiColorEditFlags_ as ImGuiColorEditFlags {} deriving (Show, Eq) #}

-- High level functions
createContext :: Maybe ImFontAtlas -> IO ImGuiContext
createContext fontAtlas = createContext_ $ fromMaybe nullPtr fontAtlas 

-- Utility functions
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

cToFloat :: (Real i, Fractional e) => i -> e
cToFloat = realToFrac

peekIntegral :: (Integral a, Storable a, Integral b) => Ptr a -> IO b
peekIntegral = (fromIntegral <$>) . peek

peekReal :: (Real a, Storable a, Fractional b) => Ptr a -> IO b
peekReal = fmap realToFrac . peek

withArrayConvIntegral :: (Integral a, Storable a, Integral b) => [b] -> (Ptr a -> IO c) -> IO c
withArrayConvIntegral = withArray . map fromIntegral

withArrayConvReal :: [Float] -> (Ptr CFloat -> IO a) -> IO a
withArrayConvReal = withArray . map realToFrac

peekIntegralArray :: (Integral a, Storable a, Num b) => Int -> Ptr a -> IO [b]
peekIntegralArray n = fmap (map fromIntegral) . peekArray n

peekIntegralArray2 :: (Integral a, Storable a, Num b) => Ptr a -> IO [b]
peekIntegralArray2 = peekIntegralArray 2

peekIntegralArray3 :: (Integral a, Storable a, Num b) => Ptr a -> IO [b]
peekIntegralArray3 = peekIntegralArray 3

peekIntegralArray4 :: (Integral a, Storable a, Num b) => Ptr a -> IO [b]
peekIntegralArray4 = peekIntegralArray 4

peekRealArray :: (Real a, Storable a, Fractional b) => Int -> Ptr a -> IO [b]
peekRealArray n = fmap (map realToFrac) . peekArray n

peekRealArray2 :: (Real a, Storable a, Fractional b) => Ptr a -> IO [b]
peekRealArray2 = peekRealArray 2

peekRealArray3 :: (Real a, Storable a, Fractional b) => Ptr a -> IO [b]
peekRealArray3 = peekRealArray 3

peekRealArray4 :: (Real a, Storable a, Fractional b) => Ptr a -> IO [b]
peekRealArray4 = peekRealArray 4
