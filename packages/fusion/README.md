⚠️WARNING⚠️ : this is beta software, and while it has been extensively tested, it probably still contains bugs (in particular, editing lists is not fully fixed - editing dictionaries is fine).

## Structure
This repository contains two things:
1. the "library" in `src`,
2. the elm-pages script to run the codegen in `scripts`.

## Setup

### Once
1. Clone this repository
2. Add the `src` folder to your `elm.json`'s `source-directories`
3. In the `scripts` folder
   1. `yarn`
   2. `yarn elm-codegen install`

### Every time you change your types
Example (ran from your project's folder):
```sh
npx elm-pages run path/to/scripts/src/Main.elm elm.json generated Types.UserDb
```

General structure (ran from anywhere):
```sh
npx elm-pages run path/to/scripts/src/Main.elm path/to/your/elm.json path/to/directory/you/want/generated/code/in The.List.Of.Types You.Want.To.Edit Defaults.To Types.BackendType
```

## Using the editor
The editor is `Fusion.Editor.value`. It takes two arguments:

`Config` is the configuration:
- `typeDict` is in the generated code at `Fusion.Generated.TypeDict`, it's used when editing custom types (it mostly tells the editor what other constructors are there and what types they have),
  - this, in prod, should be passed from the backend to avoid having it in the frontend js,
  - this can be `Dict.empty` and then you have a fully working editor anyway but can't change variant on custom types;
- `type_` will be `Just Fusion.Generated.TypeDict.Types.type_BackendModel` (or similar if you're editing a different type),
  - this can be `Nothing` but you'll have the same limitation as above;
- `editMsg` is used for editing;
  - It contains a `Patch`, which you can use it with `Fusion.Patch.patch` to apply the edit to the `Value` you have in your frontend, or send it to the backend and use `Fusion.Generated.Types.Backend.patch_BackendModel` to apply it to the real backend model.
- `queryMsg` will be used for pagination (it's currently unused).

`Value` is your model represented in a generic way. To convert your `BackendModel` to a `Value` it you can use `Fusion.Generated.Types.toValue_BackendModel`. You should do the conversion on the backend, so that the frontend JS doesn't need to know about all the types that are used in the backend.

### Supporting custom types
If you have custom type which is opaque, fusion will not generate an editor for it. You need to create a module called `Fusion.X` where `X` is the original module name and create all the functions that fusion needs (`toValue_YourType`, `patch_YourType`, `build_YourType`, `patcher_YourType`). You can look at the generated ones for inspiration. In general you'll probably want to convert your value to some other representation (think `Dict.toList`/`Dict.fromList`) and edit that.
