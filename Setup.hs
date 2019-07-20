import           Distribution.Simple
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils (rawSystemExit)
import           Distribution.Verbosity

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
  runCommand verbosity ["mkdir", "-p", "external/lib"]
  runCommand verbosity ["cp", "external/cimgui/cimgui.dylib", "external/lib/libcimgui.dylib"]

deleteCImguiLib :: Verbosity -> IO ()
deleteCImguiLib verbosity = do
  runCommand verbosity ["rm", "external/lib/libcimgui.dylib"]

runCommand :: Verbosity -> [String] -> IO ()
runCommand verbosity command =
  rawSystemExit verbosity "env" command


