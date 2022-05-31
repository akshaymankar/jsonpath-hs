let Concourse = ./deps/concourse.dhall

let Prelude = ./deps/prelude.dhall

let Git = ./deps/git.dhall

let mainBranch =
      Concourse.schemas.Resource::{
      , name = "main"
      , type = Concourse.Types.ResourceType.InBuilt "git"
      , icon = Some "git"
      , source =
          Git.Source.render
            Git.Source::{ uri = "https://github.com/akshaymankar/jsonpath-hs" }
      }

let prResource =
      Concourse.schemas.CustomResourceType::{
      , name = "github-pr"
      , type = "registry-image"
      , source = Some
          ( toMap
              { repository = Prelude.JSON.string "teliaoss/github-pr-resource" }
          )
      }

let pr =
      Concourse.schemas.Resource::{
      , name = "pr"
      , type = Concourse.Types.ResourceType.Custom prResource
      , icon = Some "source-pull"
      , source = Some
          ( toMap
              { repository = Prelude.JSON.string "akshaymankar/jsonpath-hs"
              , access_token = Prelude.JSON.string "((github-access-token))"
              , labels = Prelude.JSON.array [ Prelude.JSON.string "ok-to-test" ]
              }
          )
      }

let runTestsWith = ./run-tests-with.dhall

let runOrmolu = ./run-ormolu.dhall

let ghcs = [ "ghc922", "ghc902", "ghc8107", "ghc884" ]

let mainBranchJob =
      Concourse.schemas.Job::{
      , name = "main"
      , plan =
        [ Concourse.helpers.getStep
            Concourse.schemas.GetStep::{
            , resource = mainBranch
            , trigger = Some True
            }
        , Concourse.helpers.inParallelStep
            ( (Concourse.Types.InParallelStep Concourse.Types.Step).Config
                { steps =
                      [ runOrmolu mainBranch ]
                    # Prelude.List.map
                        Text
                        Concourse.Types.Step
                        (runTestsWith mainBranch)
                        ghcs
                , limit = Some 1
                , fail_fast = Some False
                }
            )
        ]
      }

let markCheckPending =
      λ(checks : List Text) →
        let puts =
              Prelude.List.map
                Text
                Concourse.Types.Step
                ( λ(check : Text) →
                    Concourse.helpers.putStep
                      Concourse.schemas.PutStep::{
                      , put = Some "mark-pending-${check}"
                      , resource = pr
                      , inputs = Some [ pr.name ]
                      , params = Some
                          ( toMap
                              { path = Prelude.JSON.string pr.name
                              , status = Prelude.JSON.string "PENDING"
                              , context = Prelude.JSON.string check
                              , description =
                                  Prelude.JSON.string
                                    "waiting for tests to finsih"
                              }
                          )
                      }
                )
                checks

        in  Concourse.helpers.inParallelStepSimple puts

let markCheckSuccess =
      λ(check : Text) →
        Concourse.helpers.putStep
          Concourse.schemas.PutStep::{
          , put = Some "mark-success-${check}"
          , resource = pr
          , inputs = Some [ pr.name ]
          , params = Some
              ( toMap
                  { path = Prelude.JSON.string pr.name
                  , status = Prelude.JSON.string "SUCCESS"
                  , context = Prelude.JSON.string check
                  }
              )
          }

let markCheckFailure =
      λ(check : Text) →
        Concourse.helpers.putStep
          Concourse.schemas.PutStep::{
          , put = Some "mark-failure-${check}"
          , resource = pr
          , inputs = Some [ pr.name ]
          , params = Some
              ( toMap
                  { path = Prelude.JSON.string pr.name
                  , status = Prelude.JSON.string "FAILURE"
                  , context = Prelude.JSON.string check
                  }
              )
          }

let runPRTestsWithHooks =
      λ(testStep : Concourse.Types.Step) →
      λ(checkName : Text) →
        Concourse.helpers.addHooks
          testStep
          Concourse.schemas.StepHooks::{
          , on_success = Some (markCheckSuccess checkName)
          , on_failure = Some (markCheckFailure checkName)
          }

let prJob =
      Concourse.schemas.Job::{
      , name = "pull-requests"
      , plan =
        [ Concourse.helpers.getStep
            Concourse.schemas.GetStep::{ resource = pr, trigger = Some True }
        , markCheckPending ([ "ormolu" ] # ghcs)
        , Concourse.helpers.inParallelStep
            ( (Concourse.Types.InParallelStep Concourse.Types.Step).Config
                { steps =
                      [ runPRTestsWithHooks (runOrmolu pr) "ormolu" ]
                    # Prelude.List.map
                        Text
                        Concourse.Types.Step
                        ( λ(ghc : Text) →
                            runPRTestsWithHooks (runTestsWith pr ghc) ghc
                        )
                        ghcs
                , limit = Some 1
                , fail_fast = Some False
                }
            )
        ]
      }

in  Concourse.render.pipeline [ mainBranchJob, prJob ]
