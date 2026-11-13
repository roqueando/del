-- | Custom build setup.
--
-- Resolves the relative @include-dirs@ and @extra-lib-dirs@ of the library to
-- absolute paths before configuration. Cabal registers absolute paths in the
-- package database, and @ghc-pkg@ refuses relative @library-dirs@.
module Main (main) where

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import System.Directory (getCurrentDirectory)
import System.FilePath (isAbsolute, (</>))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {confHook = absolutizeConfHook}

absolutizeConfHook
  :: (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
absolutizeConfHook (gpd, hbi) flags = do
  here <- getCurrentDirectory
  confHook simpleUserHooks (absolutize here gpd, hbi) flags

-- Resolve the library's relative include/lib directories against the package
-- root, leaving absolute paths untouched.
absolutize :: FilePath -> GenericPackageDescription -> GenericPackageDescription
absolutize here gpd =
  gpd {condLibrary = fmap (fmap absLib) (condLibrary gpd)}
  where
    absLib :: Library -> Library
    absLib lib = lib {libBuildInfo = absBuildInfo (libBuildInfo lib)}

    absBuildInfo :: BuildInfo -> BuildInfo
    absBuildInfo bi =
      bi
        { includeDirs = map absPath (includeDirs bi),
          extraLibDirs = map absPath (extraLibDirs bi)
        }

    absPath :: FilePath -> FilePath
    absPath p
      | isAbsolute p = p
      | otherwise = here </> p
