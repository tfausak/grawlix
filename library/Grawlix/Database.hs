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


insertDependencyLibrary :: Sql.Query (DependencyId, LibraryId) ()
insertDependencyLibrary = makeQuery
  [Quotes.string|
    insert into dependencies_libraries ( dependency_id, library_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


insertLibrary :: Sql.Query (PackageId, LibraryName, Conditions) LibraryId
insertLibrary = makeQuery
  [Quotes.string|
    insert into libraries ( package_id, name, conditions )
    values ( $1, $2, $3 )
    on conflict ( package_id, name, conditions ) do update
    set package_id = excluded.package_id
    returning id
  |]
  (Contravariant.contrazip3 idParam taggedTextParam taggedTextParam)
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


insertRepoType :: Sql.Query RepoType RepoTypeId
insertRepoType = makeQuery
  [Quotes.string|
    insert into repo_types ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  taggedTextParam
  idResult


insertRepoKind :: Sql.Query RepoKind RepoKindId
insertRepoKind = makeQuery
  [Quotes.string|
    insert into repo_kinds ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  taggedTextParam
  idResult


insertPackageRepo :: Sql.Query (PackageId, RepoId) ()
insertPackageRepo = makeQuery
  [Quotes.string|
    insert into packages_repos ( package_id, repo_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2 idParam idParam)
  Sql.Decode.unit


insertRepo :: Sql.Query (RepoKindId, RepoTypeId, Text.Text) RepoId
insertRepo = makeQuery
  [Quotes.string|
    insert into repos ( repo_kind_id, repo_type_id, url )
    values ( $1, $2, $3 )
    on conflict ( repo_kind_id, repo_type_id, url ) do update
    set repo_kind_id = excluded.repo_kind_id
    returning id
  |]
  (Contravariant.contrazip3 idParam idParam textParam)
  idResult


insertDependency :: Sql.Query (ConstraintId, PackageNameId) DependencyId
insertDependency = makeQuery
  [Quotes.string|
    insert into dependencies ( constraint_id, package_name_id )
    values ( $1, $2 )
    on conflict ( constraint_id, package_name_id ) do update
    set constraint_id = excluded.constraint_id
    returning id
  |]
  (Contravariant.contrazip2 idParam idParam)
  idResult


insertConstraint :: Sql.Query Constraint ConstraintId
insertConstraint = makeQuery
  [Quotes.string|
    insert into constraints ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  taggedTextParam
  idResult


insertModuleName :: Sql.Query ModuleName ModuleNameId
insertModuleName = makeQuery
  [Quotes.string|
    insert into module_names ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  (Sql.Encode.text |> arrayOf |> contraUntag)
  idResult


insertPackageName :: Sql.Query PackageName PackageNameId
insertPackageName = makeQuery
  [Quotes.string|
    insert into package_names ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  taggedTextParam
  idResult


insertVersion :: Sql.Query Version VersionId
insertVersion = makeQuery
  [Quotes.string|
    insert into versions ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  (Sql.Encode.int4 |> arrayOf |> contraUntag)
  idResult


insertLicense :: Sql.Query License LicenseId
insertLicense = makeQuery
  [Quotes.string|
    insert into licenses ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
  |]
  taggedTextParam
  idResult


insertPackage
  :: Sql.Query
    ( PackageNameId
    , VersionId
    , Revision
    , LicenseId
    , Text.Text
    , Text.Text
    , Text.Text
    )
    PackageId
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
      $1,
      $2,
      $3,
      $4,
      $5,
      $6,
      $7
    ) on conflict (
      package_name_id,
      version_id,
      revision
    ) do update
    set package_name_id = excluded.package_name_id
    returning id
  |]
  (Contravariant.contrazip7
    idParam
    idParam
    idParam
    idParam
    textParam
    textParam
    textParam)
  idResult


insertCategory :: Sql.Query Category CategoryId
insertCategory = makeQuery
  [Quotes.string|
    insert into categories ( content )
    values ( $1 )
    on conflict ( content ) do update
    set content = excluded.content
    returning id
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
