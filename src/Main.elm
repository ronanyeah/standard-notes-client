module Main exposing (main)

import Http
import Json
import Json.Decode as Decode
import Navigation exposing (Location)
import Types exposing (..)
import Update exposing (update)
import Utils exposing (emptyModel, goTo, log, useResult)
import View exposing (view)
import Window


main : Program (Maybe String) Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , subscriptions =
            \_ ->
                Window.resizes WindowSize
        , update = update
        , view = view
        }


init : Maybe String -> Location -> ( Model, Cmd Msg )
init maybeData location =
    maybeData
        |> Result.fromMaybe "No Auth Provided"
        |> Result.andThen (Decode.decodeString Json.decodeAuth)
        |> useResult
            (\err ->
                ( emptyModel
                , Cmd.batch
                    [ log err
                    , goTo LoginRoute
                    , Http.post "/crypto/check" Http.emptyBody (Decode.value |> Decode.map (always ()))
                        |> Http.send (ServiceWorkerCheck location)
                    ]
                )
            )
            (\auth ->
                ( { emptyModel
                    | auth = Just auth
                  }
                , Http.post "/crypto/check" Http.emptyBody (Decode.value |> Decode.map (always ()))
                    |> Http.send (ServiceWorkerCheck location)
                )
            )
