import           Distribution.Simple
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils (copyFileVerbose, die', findFileWithExtension,
                                            rawSystemExit)
import           Distribution.Verbosity
import           System.FilePath
main = defaultMainWithHooks simpleUserHooks
    { preConf = \args flags -> do
        let verbosity = (fromFlag $ configVerbosity flags)
        putStrLn "Make all CImgui lib"
        makeCImguiLib verbosity "all"
        putStrLn "Copying and renaming CImgui lib to external/lib"
        copyCImguiLib verbosity
        preConf simpleUserHooks args flags
    , preClean = \args flags -> do
        let verbosity = (fromFlag $ cleanVerbosity flags)
        putStrLn "Make clean CImgui lib"
        makeCImguiLib verbosity "clean"
        putStrLn "Removing CImgui lib from external/lib"
        deleteCImguiLib verbosity
        preClean simpleUserHooks args flags
    }

makeCImguiLib :: Verbosity -> String -> IO ()
makeCImguiLib verbosity makeCommand = do
  runCommand verbosity ["make", "--directory=external/cimgui", makeCommand]

copyCImguiLib :: Verbosity -> IO ()
copyCImguiLib verbosity = do
  file <- findFileWithExtension ["so", "dylib", "dll"] ["external/cimgui"] "cimgui"
  case file of
    Just lib ->
      case takeFileName lib of
        "cimgui.dylib" -> copyFileVerbose verbosity lib "external/lib/libcimgui.dylib"
        "cimgui.so"    -> copyFileVerbose verbosity lib "external/lib/libcimgui.so"
        "cimgui.dll"   -> copyFileVerbose verbosity lib "external/lib/libcimgui.dll"
    Nothing  -> die' verbosity "Could not find library"

deleteCImguiLib :: Verbosity -> IO ()
deleteCImguiLib verbosity = do
  runCommand verbosity ["rm", "external/lib"]

runCommand :: Verbosity -> [String] -> IO ()
runCommand verbosity command =
  rawSystemExit verbosity "env" command


