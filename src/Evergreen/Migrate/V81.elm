module Evergreen.Migrate.V81 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Evergreen.V80.Types
import Evergreen.V81.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V80.Types.FrontendModel -> ModelMigration Evergreen.V81.Types.FrontendModel Evergreen.V81.Types.FrontendMsg
frontendModel old =
    ModelReset


backendModel : Evergreen.V80.Types.BackendModel -> ModelMigration Evergreen.V81.Types.BackendModel Evergreen.V81.Types.BackendMsg
backendModel old =
    ModelReset


frontendMsg : Evergreen.V80.Types.FrontendMsg -> MsgMigration Evergreen.V81.Types.FrontendMsg Evergreen.V81.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V80.Types.ToBackend -> MsgMigration Evergreen.V81.Types.ToBackend Evergreen.V81.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V80.Types.BackendMsg -> MsgMigration Evergreen.V81.Types.BackendMsg Evergreen.V81.Types.BackendMsg
backendMsg old =
    MsgOldValueIgnored


toFrontend : Evergreen.V80.Types.ToFrontend -> MsgMigration Evergreen.V81.Types.ToFrontend Evergreen.V81.Types.FrontendMsg
toFrontend old =
    MsgOldValueIgnored
