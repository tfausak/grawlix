module Grawlix.Type.Package
  ( Package(..)
  ) where

import Grawlix.Type.Benchmarks
import Grawlix.Type.Categories
import Grawlix.Type.Description
import Grawlix.Type.Executables
import Grawlix.Type.Libraries
import Grawlix.Type.License
import Grawlix.Type.PackageName
import Grawlix.Type.PackageUrl
import Grawlix.Type.Repos
import Grawlix.Type.Revision
import Grawlix.Type.Synopsis
import Grawlix.Type.Tests
import Grawlix.Type.Version

data Package = Package
  { packageName :: PackageName
  , packageVersion :: Version
  , packageRevision :: Revision
  , packageLicense :: License
  , packageSynopsis :: Synopsis
  , packageDescription :: Description
  , packageCategories :: Categories
  , packageUrl :: PackageUrl
  , packageRepos :: Repos
  , packageLibraries :: Libraries
  , packageExecutables :: Executables
  , packageTests :: Tests
  , packageBenchmarks :: Benchmarks
  } deriving (Eq, Show)
