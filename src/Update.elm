module Update exposing (update)

import Dict
import Http
import Json
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Router exposing (router)
import Task exposing (Task)
import Types exposing (..)
import Utils exposing (goTo, isCode, log, sync, unwrap)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoTo route ->
            ( model, goTo route )

        UpdatePassword str ->
            ( { model | password = str }, Cmd.none )

        UpdateUsername str ->
            ( { model | username = str }, Cmd.none )

        Submit ->
            ( model
            , Http.get
                ("https://sync.standardnotes.org/auth/params?email="
                    ++ Http.encodeUri model.username
                )
                Json.decodeParams
                |> Http.toTask
                |> Task.andThen
                    (\{ cost, salt } ->
                        Http.post
                            "/crypto/key"
                            (Encode.object
                                [ ( "salt", Encode.string salt )
                                , ( "password", Encode.string model.password )
                                , ( "cost", Encode.int cost )
                                ]
                                |> Http.jsonBody
                            )
                            Decode.string
                            |> Http.toTask
                    )
                |> Task.andThen
                    (\str ->
                        let
                            ( pw, encryptionKey, authKey ) =
                                splitIntoThree str
                        in
                        Http.post
                            "https://sync.standardnotes.org/auth/sign_in"
                            (Encode.object
                                [ ( "email", Encode.string model.username )
                                , ( "password", Encode.string pw )
                                ]
                                |> Http.jsonBody
                            )
                            (Decode.field "token" Decode.string
                                |> Decode.map
                                    (\token ->
                                        { token = token
                                        , encryptionKey = encryptionKey
                                        , authKey = authKey
                                        }
                                    )
                            )
                            |> Http.toTask
                    )
                |> Task.attempt SignInCb
            )

        SignInCb res ->
            case res of
                Ok auth ->
                    ( { model
                        | auth = Just auth
                      }
                    , Cmd.batch
                        [ sync [] model.syncToken auth.token
                            |> Http.send (SyncCb auth)
                        , Encode.object
                            [ ( "token", Encode.string auth.token )
                            , ( "authKey", Encode.string auth.authKey )
                            , ( "encryptionKey", Encode.string auth.encryptionKey )
                            ]
                            |> Encode.encode 0
                            |> Ports.saveAuth
                        , goTo NotesRoute
                        ]
                    )

                Err err ->
                    ( model, log err )

        UpdateText str ->
            ( { model | text = str }, Cmd.none )

        UpdateTitle str ->
            ( { model | title = str }, Cmd.none )

        SaveNote ({ authKey, encryptionKey, token } as auth) note ->
            let
                cmd =
                    Http.post
                        "/crypto/encrypt-item"
                        (Encode.object
                            [ ( "uuid", Encode.string note.uuid )
                            , ( "authKey", Encode.string authKey )
                            , ( "encryptionKey", Encode.string encryptionKey )
                            , ( "data"
                              , Encode.object <|
                                    [ ( "title", Encode.string model.title )
                                    , ( "text", Encode.string model.text )
                                    , ( "references", Encode.list [] )
                                    , ( "appData", Encode.object [] )
                                    ]
                              )
                            ]
                            |> Http.jsonBody
                        )
                        (Decode.map2 (,)
                            (Decode.field "encryptedContent" Decode.string)
                            (Decode.field "encItemKey" Decode.string)
                        )
                        |> Http.toTask
                        |> Task.andThen
                            (\( encryptedContent, encItemKey ) ->
                                sync
                                    [ { uuid = note.uuid
                                      , content = Just encryptedContent
                                      , contentType = "Note"
                                      , encItemKey = Just encItemKey
                                      , deleted = False
                                      , createdAt = note.createdAt
                                      , updatedAt = note.updatedAt
                                      }
                                    ]
                                    model.syncToken
                                    token
                                    |> Http.toTask
                            )
                        |> Task.attempt (SyncCb auth)
            in
            ( model, Cmd.batch [ cmd, goTo NotesRoute ] )

        ServiceWorkerCheck location res ->
            case res of
                Ok () ->
                    update (UrlChange location) model
                        |> Tuple.mapSecond
                            (\cmd ->
                                Cmd.batch
                                    [ cmd
                                    , model.auth
                                        |> unwrap Cmd.none
                                            (\auth ->
                                                sync [] Nothing auth.token
                                                    |> Http.send (SyncCb auth)
                                            )
                                    ]
                            )

                Err err ->
                    if isCode 404 err then
                        ( { model
                            | view = ServiceWorkerFailView
                          }
                        , Cmd.none
                        )
                    else
                        ( model
                        , Cmd.batch
                            [ goTo LoginRoute
                            , log err
                            ]
                        )

        SyncCb { authKey, encryptionKey } res ->
            case res of
                Ok { retrievedItems, savedItems, syncToken } ->
                    ( { model | syncToken = syncToken }
                    , (savedItems ++ retrievedItems)
                        |> List.map (decrypt authKey encryptionKey)
                        |> Task.sequence
                        |> Task.map
                            (List.filterMap
                                (\x ->
                                    case x of
                                        NoteContent note ->
                                            Just ( note.uuid, note )

                                        _ ->
                                            Nothing
                                )
                                >> Dict.fromList
                                >> flip Dict.union model.notes
                            )
                        |> Task.attempt UpdateNotes
                    )

                Err err ->
                    ( model
                    , Cmd.batch
                        [ log err
                        , if isCode 500 err then
                            Cmd.batch
                                [ Ports.clearAuth ()
                                , goTo LoginRoute
                                ]
                          else
                            Cmd.none
                        ]
                    )

        UpdateNotes res ->
            case res of
                Ok notes ->
                    ( { model | notes = notes }, Cmd.none )

                Err err ->
                    ( model, log err )

        UrlChange location ->
            case router location of
                NoteRoute id ->
                    id
                        |> flip Dict.get model.notes
                        |> unwrap
                            ( model, goTo NotesRoute )
                            (\note ->
                                ( { model
                                    | title = note.title
                                    , text = note.text
                                    , view = NoteView note
                                  }
                                , Cmd.none
                                )
                            )

                NotesRoute ->
                    ( { model | view = NotesView }, Cmd.none )

                TagRoute id ->
                    id
                        |> flip Dict.get model.tags
                        |> unwrap
                            ( model, goTo TagsRoute )
                            (\{ title } ->
                                ( { model
                                    | title = title
                                  }
                                , Cmd.none
                                )
                            )

                TagsRoute ->
                    ( { model | view = TagsView }, Cmd.none )

                LoginRoute ->
                    ( { model | view = LoginView }, Cmd.none )

                NotFoundRoute ->
                    ( model, goTo LoginRoute )

        WindowSize _ ->
            ( model, Cmd.none )

        ValueCb res ->
            ( model, log res )


splitIntoThree : String -> ( String, String, String )
splitIntoThree str =
    let
        outputLength =
            String.length str

        splitLength =
            outputLength // 3
    in
    ( String.left splitLength str
    , String.slice splitLength (splitLength * 2) str
    , String.right splitLength str
    )


decrypt : String -> String -> Item -> Task Http.Error Content
decrypt authKey encryptionKey item =
    case ( item.content, item.encItemKey ) of
        ( Just content, Just encItemKey ) ->
            Http.post
                "/crypto/decrypt-item"
                (Encode.object
                    [ ( "content", Encode.string content )
                    , ( "encItemKey", Encode.string encItemKey )
                    , ( "authKey", Encode.string authKey )
                    , ( "encryptionKey", Encode.string encryptionKey )
                    ]
                    |> Http.jsonBody
                )
                (Json.decodeContent item)
                |> Http.toTask

        _ ->
            Task.succeed <| Other item
