import           Data.Maybe                         (fromJust)
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Verbosity
import           System.Directory                   (getCurrentDirectory)
import           System.FilePath                    (takeFileName)
import           System.Info

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    preConf = preConf',
    confHook = confHook',
    preBuild = preBuild',
    postCopy = postCopy'
  }

preConf' :: Args -> ConfigFlags -> IO HookedBuildInfo
preConf' args flags = do
  let verbosity = (fromFlag $ configVerbosity flags)
  putStrLn "Make all CImgui lib"
  runCommand verbosity ["make", "--directory=external/cimgui", "all"]
  putStrLn "Copying and renaming CImgui lib to external/lib"
  copyCImguiLib verbosity
  preConf simpleUserHooks args flags

confHook' :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
confHook' (description, buildInfo) flags = do
    lbi <- confHook simpleUserHooks (description, buildInfo) flags
    dir <- getCurrentDirectory
    let lpd   = localPkgDescr lbi
    let lib   = fromJust (library lpd)
    let libbi = libBuildInfo lib
    let libPref = libdir $ absoluteInstallDirs lpd lbi NoCopyDest

    let libbi' = libbi {
      extraLibDirs = libPref : extraLibDirs libbi ++ [(dir ++ "/external/lib")]
    }

    let lib' = lib { libBuildInfo = libbi' }
    let lpd' = lpd { library = Just lib' }

    return $ lbi { localPkgDescr = lpd' }


preBuild' :: Args -> BuildFlags -> IO HookedBuildInfo
preBuild' _ _ = do
    dir <- getCurrentDirectory
    let extLibDir = dir ++ "/external/lib"
    putStrLn $ "preBuild: adding " ++ extLibDir ++ "/external/lib into extraLibDirs"
    let bi = emptyBuildInfo { extraLibDirs = [ extLibDir ] }
    return (Just bi, [])


postCopy' :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopy' _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    let from = ("external/lib/libcimgui" ++ ext)
    let to = (libPref ++ "/libcimgui" ++ ext)
    installExecutableFile verbosity from to


copyCImguiLib :: Verbosity -> IO ()
copyCImguiLib verbosity = do
  createDirectoryIfMissingVerbose verbosity True "external/lib"
  let from = ("external/cimgui/cimgui" ++ ext)
  let to = ("external/lib/libcimgui" ++ ext)
  copyFileVerbose verbosity from to

runCommand :: Verbosity -> [String] -> IO ()
runCommand verbosity command =
  rawSystemExit verbosity "env" command

ext :: String
ext = case os of
  "darwin"  -> ".dylib"
  "windows" -> ".dll"
  _         -> ".so"


