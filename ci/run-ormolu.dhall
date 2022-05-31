let Concourse = ./deps/concourse.dhall

let Prelude = ./deps/prelude.dhall

let runTestsWith
    : Concourse.Types.Resource -> Concourse.Types.Step
    = \(repo : Concourse.Types.Resource) ->
        Concourse.helpers.taskStep
          Concourse.schemas.TaskStep::{
          , task = "run-ormolu"
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
                  , path = "bash"
                  , args = Some
                    [ "-c"
                    , ./run-ormolu.sh as Text
                    , let dollarZero = "run-ormolu.sh" in dollarZero
                    , repo.name
                    ]
                  }
                }
          }

in  runTestsWith
