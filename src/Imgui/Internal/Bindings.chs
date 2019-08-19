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
{#pointer *ImGuiWindowFlags as ImGuiWindowFlags  #}

-- Imgui functions
{# fun igGetVersion as getVersion { } -> `String' #}
{# fun igCreateContext as createContext_ { `ImFontAtlas'  } -> `ImGuiContext' #}
{# fun igGetIO as getIO { } -> `ImGuiIO' #}
{# fun igNewFrame as newFrame { } -> `()' #}
{# fun igRender as render { } -> `()' #}
{# fun igGetDrawData as getDrawData { } -> `ImDrawData' #}
{# fun igBegin as begin { `String', `Int', `Int' } -> `()' #}
{# fun igEnd as end { } -> `()' #}
{# fun igButton as button {`String', %`ImVec2Ptr'} -> `Bool'#}
{# fun igText as text { `String' } -> `()' #}

{# fun igShowDemoWindow as showDemoWindow { `Bool' } -> `()' #}
{# fun igShowMetricsWindow as showMetricsWindow { `Bool' } -> `()' #}
{# fun igShowAboutWindow as showAboutWindow { `Bool' } -> `()' #}
{# fun igShowUserGuide as showUserGuide { } -> `()' #}


{# fun igPushStyleColor as pushStyleColor { cFromEnum `ImGuiCol', %`ImVec4Ptr' } -> `()' #}
{# fun igPopStyleColor as popStyleColor { `Int' } -> `()' #}


{#fun unsafe igSliderInt as sliderInt
  { `String'
  , `Int' peekIntegral*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt2 as sliderInt2
  { `String'
  , withArrayConvI * `[Int]' peekArray2*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt3 as sliderInt3
  { `String'
  , withArrayConvI * `[Int]' peekArray3*
  , `Int'
  , `Int'
  , `String'
  } -> `()' #}

{#fun unsafe igSliderInt4 as sliderInt4
  { `String'
  , withArrayConvI * `[Int]' peekArray4*
  , `Int'
  , `Int'
  , `String'
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

-- | Peek from pointer then cast to another integral type.
peekIntegral :: (Integral a, Storable a, Integral b) => Ptr a -> IO b
peekIntegral = (fromIntegral <$>) . peek

{-# SPECIALIZE peekIntegral :: Ptr CInt -> IO Int #-}

withArrayConvI :: [Int] -> (Ptr CInt -> IO a) -> IO a
withArrayConvI = withArray . map fromIntegral

peekArrayN :: Int -> Ptr CInt -> IO [Int]
peekArrayN n d = map fromIntegral `fmap` peekArray n d

peekArray2 :: Ptr CInt -> IO [Int]
peekArray2 d = peekArrayN 2 d

peekArray3 :: Ptr CInt -> IO [Int]
peekArray3 d = peekArrayN 3 d

peekArray4 :: Ptr CInt -> IO [Int]
peekArray4 d = peekArrayN 4 d

