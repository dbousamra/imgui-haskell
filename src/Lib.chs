{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include <cimgui.h>
#include <cimgui_impl.h>
#include <GL/gl3w.h>

{# context lib="cimgui" #}

module Lib where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Data.Maybe (fromMaybe)
import qualified SDL.Raw.Types           as SDL
import qualified SDL.Internal.Types           as SDL hiding (Window)


{#pointer *ImVec2 as ImVec2Ptr foreign newtype#}
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
{# fun igShowDemoWindow as showDemoWindow { `Int' } -> `()' #}

-- Creating structs
{#fun pure ImVec2_ImVec2Float as makeImVec2 {`Float', `Float'} -> `ImVec2Ptr'#}

-- ImGui Impl functions
{# fun gl3wInit as gl3wInit { } -> `()' #}
{# fun ImGui_ImplOpenGL3_Init as openGL3Init { `String' } -> `Bool' #}
{# fun ImGui_ImplOpenGL3_NewFrame as openGL3NewFrame { } -> `()' #}
{# fun ImGui_ImplOpenGL3_RenderDrawData as openGL3RenderDrawData { `ImDrawData' } -> `()' #}
{# fun ImGui_ImplSDL2_InitForOpenGL as initForOpenGL { castPtr `SDL.Window', castPtr `SDL.GLContext' } -> `Bool' #}
{# fun ImGui_ImplSDL2_NewFrame as sdl2NewFrame { castPtr `SDL.Window' } -> `()' #}
{# fun ImGui_ImplSDL2_Shutdown as shutdown { } -> `()' #}

-- High level functions
createContext :: Maybe ImFontAtlas -> IO ImGuiContext
createContext fontAtlas = createContext_ $ fromMaybe nullPtr fontAtlas 


