module Evergreen.Migrate.V83 exposing (..)

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

import Evergreen.V81.Types
import Evergreen.V83.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V81.Types.FrontendModel -> ModelMigration Evergreen.V83.Types.FrontendModel Evergreen.V83.Types.FrontendMsg
frontendModel old =
    ModelReset


backendModel : Evergreen.V81.Types.BackendModel -> ModelMigration Evergreen.V83.Types.BackendModel Evergreen.V83.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V81.Types.FrontendMsg -> MsgMigration Evergreen.V83.Types.FrontendMsg Evergreen.V83.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V81.Types.ToBackend -> MsgMigration Evergreen.V83.Types.ToBackend Evergreen.V83.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V81.Types.BackendMsg -> MsgMigration Evergreen.V83.Types.BackendMsg Evergreen.V83.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V81.Types.ToFrontend -> MsgMigration Evergreen.V83.Types.ToFrontend Evergreen.V83.Types.FrontendMsg
toFrontend old =
    MsgUnchanged
