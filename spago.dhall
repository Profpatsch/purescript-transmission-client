{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "my-project"
, dependencies =
    [ "effect"
    , "console"
    , "milkis"
    , "aff"
    , "psci-support"
    , "undefined"
    , "simple-json"
    , "assert"
    , "generics-rep"
    ]
, packages =
    ./packages.dhall
}
