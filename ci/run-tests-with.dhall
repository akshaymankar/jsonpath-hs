let Concourse = ./deps/concourse.dhall

let Prelude = ./deps/prelude.dhall

let runTestsWith
    : Concourse.Types.Resource -> Text -> Concourse.Types.Step
    = \(repo : Concourse.Types.Resource) ->
      \(ghc : Text) ->
        Concourse.helpers.taskStep
          Concourse.schemas.TaskStep::{
          , task = "run-tests-${ghc}"
          , config =
              Concourse.Types.TaskSpec.Config
                Concourse.schemas.TaskConfig::{
                , image_resource = Some Concourse.schemas.ImageResource::{
                  , type = "registry-image"
                  , source = Some
                      ( toMap
                          { repository = Prelude.JSON.string "nixos/nix"
                          , tag = Prelude.JSON.string "latest"
                          }
                      )
                  }
                , inputs = Some
                  [ Concourse.schemas.TaskInput::{ name = repo.name } ]
                , params = Some
                    (toMap { CACHIX_AUTH_TOKEN = Some "((cachix-token))" })
                , run = Concourse.schemas.TaskRunConfig::{
                  , path = "sh"
                  , args = Some
                    [ "-c"
                    , ./compile-and-test.sh as Text
                    , let dollarZero = "compile-and-test.sh" in dollarZero
                    , repo.name
                    , ghc
                    ]
                  }
                }
          }

in  runTestsWith
