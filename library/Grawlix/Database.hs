{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Database where

import Flow ((|>))
import Grawlix.Types

import qualified Contravariant.Extras as Contravariant
import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Grawlix.Quotes as Quotes
import qualified Hasql.Connection as Sql
import qualified Hasql.Decoders as Sql.Decode
import qualified Hasql.Encoders as Sql.Encode
import qualified Hasql.Migration as Sql
import qualified Hasql.Query as Sql
import qualified Hasql.Session as Sql
import qualified Hasql.Transaction as Sql.Transaction
import qualified Hasql.Transaction.Sessions as Sql
import qualified System.Environment as Environment


selectDependencyId :: Sql.Query (ConstraintId, PackageNameId) DependencyId
selectDependencyId = makeQuery
  [Quotes.string|
    select id
    from dependencies
    where constraint_id = $1
    and package_name_id = $2
  |]
  (Contravariant.contrazip2 idParam idParam)
  idResult


insertDependencyLibrary :: Sql.Query (DependencyId, LibraryId) ()
insertDependencyLibrary = makeQuery
  [Quotes.string|
    insert into dependencies_libraries ( dependency_id, library_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


insertLibrary :: Sql.Query (PackageId, LibraryName, Conditions) ()
insertLibrary = makeQuery
  [Quotes.string|
    insert into libraries ( package_id, name, conditions )
    values ( $1, $2, $3 )
    on conflict do nothing
  |]
  (Contravariant.contrazip3 idParam taggedTextParam taggedTextParam)
  Sql.Decode.unit


selectLibraryId :: Sql.Query (PackageId, LibraryName, Conditions) LibraryId
selectLibraryId = makeQuery
  [Quotes.string|
    select id
    from libraries
    where package_id = $1
    and name = $2
    and conditions = $3
  |]
  (Contravariant.contrazip3 idParam taggedTextParam taggedTextParam)
  idResult


selectModuleNameId :: Sql.Query ModuleName ModuleNameId
selectModuleNameId = makeQuery
  [Quotes.string|
    select id
    from module_names
    where content = $1
  |]
  (Sql.Encode.text |> arrayOf |> contraUntag)
  idResult


insertLibraryModuleName :: Sql.Query (LibraryId, ModuleNameId) ()
insertLibraryModuleName = makeQuery
  [Quotes.string|
    insert into libraries_module_names ( library_id, module_name_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


selectRepoTypeId :: Sql.Query RepoType RepoTypeId
selectRepoTypeId = makeQuery
  [Quotes.string|
    select id
    from repo_types
    where content = $1
  |]
  taggedTextParam
  idResult


insertRepoType :: Sql.Query RepoType ()
insertRepoType = makeQuery
  [Quotes.string|
    insert into repo_types ( content )
    values ( $1 )
    on conflict do nothing
  |]
  taggedTextParam
  Sql.Decode.unit


selectRepoKindId :: Sql.Query RepoKind RepoKindId
selectRepoKindId = makeQuery
  [Quotes.string|
    select id
    from repo_kinds
    where content = $1
  |]
  taggedTextParam
  idResult


insertRepoKind :: Sql.Query RepoKind ()
insertRepoKind = makeQuery
  [Quotes.string|
    insert into repo_kinds ( content )
    values ( $1 )
    on conflict do nothing
  |]
  taggedTextParam
  Sql.Decode.unit


insertPackageRepo :: Sql.Query (PackageId, RepoId) ()
insertPackageRepo = makeQuery
  [Quotes.string|
    insert into packages_repos ( package_id, repo_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


selectRepoId :: Sql.Query (RepoKindId, RepoTypeId, Text.Text) RepoId
selectRepoId = makeQuery
  [Quotes.string|
    select id
    from repos
    where repo_kind_id = $1
    and repo_type_id = $2
    and url = $3
  |]
  (Contravariant.contrazip3 idParam idParam textParam)
  idResult


insertRepo :: Sql.Query (RepoKindId, RepoTypeId, Text.Text) ()
insertRepo = makeQuery
  [Quotes.string|
    insert into repos ( repo_kind_id, repo_type_id, url )
    values ( $1, $2, $3 )
    on conflict do nothing
  |]
  (Contravariant.contrazip3 idParam idParam textParam)
  Sql.Decode.unit


insertDependency :: Sql.Query (ConstraintId, PackageNameId) ()
insertDependency = makeQuery
  [Quotes.string|
    insert into dependencies ( constraint_id, package_name_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


selectConstraintId :: Sql.Query Constraint ConstraintId
selectConstraintId = makeQuery
  [Quotes.string|
    select id
    from constraints
    where content = $1
  |]
  taggedTextParam
  idResult


selectPackageNameId :: Sql.Query PackageName PackageNameId
selectPackageNameId = makeQuery
  [Quotes.string|
    select id
    from package_names
    where content = $1
  |]
  taggedTextParam
  idResult


insertConstraint :: Sql.Query Constraint ()
insertConstraint = makeQuery
  [Quotes.string|
    insert into constraints ( content )
    values ( $1 )
    on conflict do nothing
  |]
  taggedTextParam
  Sql.Decode.unit


insertModuleName :: Sql.Query ModuleName ()
insertModuleName = makeQuery
  [Quotes.string|
    insert into module_names ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.text |> arrayOf |> contraUntag)
  Sql.Decode.unit


insertPackageName :: Sql.Query PackageName ()
insertPackageName = makeQuery
  [Quotes.string|
    insert into package_names ( content )
    values ( $1 )
    on conflict do nothing
  |]
  taggedTextParam
  Sql.Decode.unit


insertVersion :: Sql.Query Version ()
insertVersion = makeQuery
  [Quotes.string|
    insert into versions ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.int4 |> arrayOf |> contraUntag)
  Sql.Decode.unit


insertLicense :: Sql.Query License ()
insertLicense = makeQuery
  [Quotes.string|
    insert into licenses ( content )
    values ( $1 )
    on conflict do nothing
  |]
  taggedTextParam
  Sql.Decode.unit


insertPackage
  :: Sql.Query
    ( PackageName
    , Version
    , Revision
    , License
    , Text.Text
    , Text.Text
    , Text.Text
    )
    ()
insertPackage = makeQuery
  [Quotes.string|
    insert into packages (
      package_name_id,
      version_id,
      revision,
      license_id,
      synopsis,
      description,
      url
    ) values (
      ( select id from package_names where content = $1 ),
      ( select id from versions where content = $2 ),
      $3,
      ( select id from licenses where content = $4 ),
      $5,
      $6,
      $7
    ) on conflict do nothing
  |]
  (Contravariant.contrazip7
    taggedTextParam
    (Sql.Encode.int4 |> arrayOf |> contraUntag)
    idParam
    taggedTextParam
    textParam
    textParam
    textParam)
  Sql.Decode.unit


selectPackageId :: Sql.Query (PackageName, Version, Revision) PackageId
selectPackageId = makeQuery
  [Quotes.string|
    select packages.id
    from packages
    inner join package_names
    on package_names.id = packages.package_name_id
    inner join versions
    on versions.id = packages.version_id
    where package_names.content = $1
    and versions.content = $2
    and packages.revision = $3
  |]
  (Contravariant.contrazip3
    taggedTextParam
    (Sql.Encode.int4 |> arrayOf |> contraUntag)
    idParam)
  idResult


insertCategory :: Sql.Query Category ()
insertCategory = makeQuery
  [Quotes.string|
    insert into categories ( content )
    values ( $1 )
    on conflict do nothing
  |]
  taggedTextParam
  Sql.Decode.unit


selectCategoryId :: Sql.Query Category CategoryId
selectCategoryId = makeQuery
  [Quotes.string|
    select id
    from categories
    where content = $1
  |]
  taggedTextParam
  idResult


insertCategoryPackage :: Sql.Query (CategoryId, PackageId) ()
insertCategoryPackage = makeQuery
  [Quotes.string|
    insert into categories_packages ( category_id, package_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


makeQuery
  :: String -> Sql.Encode.Params a -> Sql.Decode.Result b -> Sql.Query a b
makeQuery rawSql encoder decoder = let
  sql = rawSql |> Text.pack |> Text.encodeUtf8
  prepare = True
  in Sql.statement sql encoder decoder prepare


runQuery :: Sql.Connection -> Sql.Query a b -> a -> IO b
runQuery connection query params = do
  let session = Sql.query params query
  result <- Sql.run session connection
  case result of
    Left problem -> fail (show problem)
    Right value -> pure value


arrayOf :: Sql.Encode.Value a -> Sql.Encode.Params [a]
arrayOf x = x
  |> Sql.Encode.arrayValue
  |> Sql.Encode.arrayDimension foldl
  |> Sql.Encode.array
  |> Sql.Encode.value


contraUntag :: Contravariant.Contravariant f => f a -> f (Tagged.Tagged t a)
contraUntag = Contravariant.contramap Tagged.untag


runMigration :: Sql.Connection -> Sql.MigrationCommand -> IO ()
runMigration connection migration = do
  let session = migration
        |> Sql.runMigration
        |> Sql.transaction Sql.Transaction.Serializable Sql.Transaction.Write
  result <- Sql.run session connection
  case result of
    Right Sql.MigrationSuccess -> pure ()
    _ -> result |> show |> fail


getConnection :: IO Sql.Connection
getConnection = do
  maybeSettings <- Environment.lookupEnv "DATABASE"
  result <- maybeSettings
    |> Maybe.fromMaybe ""
    |> Text.pack
    |> Text.encodeUtf8
    |> Sql.acquire
  case result of
    Left problem -> problem |> show |> fail
    Right connection -> pure connection


idParam :: Sql.Encode.Params (Tagged.Tagged t Int.Int32)
idParam = Sql.Encode.int4 |> Sql.Encode.value |> contraUntag


idResult :: Sql.Decode.Result (Tagged.Tagged t Int.Int32)
idResult = Sql.Decode.int4
  |> Sql.Decode.value
  |> Sql.Decode.singleRow
  |> fmap Tagged.Tagged


textParam :: Sql.Encode.Params Text.Text
textParam = Sql.Encode.value Sql.Encode.text


taggedTextParam :: Sql.Encode.Params (Tagged.Tagged t Text.Text)
taggedTextParam = contraUntag textParam
