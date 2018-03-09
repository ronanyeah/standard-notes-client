module Main exposing (main)

import Color
import Date exposing (Date)
import Element exposing (centerX, centerY, column, el, empty, fill, layoutWith, noHover, spacing, text, width)
import Element.Border as Border
import Element.Input as Input
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import Navigation exposing (Location)
import Ports
import Task exposing (Task)
import Window exposing (Size)


type Msg
    = UrlChange Location
    | SignInCb (Result Http.Error Auth)
    | ItemsCb (Result Http.Error (List Item))
    | NotesCb (Result Http.Error (List String))
    | WindowSize Size
    | UpdatePassword String
    | UpdateUsername String
    | Submit


type alias Model =
    { password : String
    , username : String
    , auth : Maybe Auth
    , items : List Item
    , notes : List String
    }


type alias Params =
    { cost : Int
    , salt : String
    }


type alias Item =
    { uuid : String
    , content : Maybe String
    , contentType : Content
    , encItemKey : Maybe String
    , deleted : Bool
    , createdAt : Date
    , updatedAt : Date
    }


type alias Auth =
    { token : String
    , encryptionKey : String
    , authKey : String
    }


decodeAuth : Decoder Auth
decodeAuth =
    Dec.map3 Auth
        (Dec.field "token" Dec.string)
        (Dec.field "encryptionKey" Dec.string)
        (Dec.field "authKey" Dec.string)


decodeItem : Decoder Item
decodeItem =
    Dec.map7 Item
        (Dec.field "uuid" Dec.string)
        (Dec.field "content" (Dec.nullable Dec.string))
        decodeContent
        (Dec.field "enc_item_key" (Dec.nullable Dec.string))
        (Dec.field "deleted" Dec.bool)
        (Dec.field "created_at"
            (Dec.string
                |> Dec.andThen
                    (Date.fromString
                        >> Result.map Dec.succeed
                        >> Result.withDefault (Dec.fail "bad date")
                    )
            )
        )
        (Dec.field "updated_at"
            (Dec.string
                |> Dec.andThen
                    (Date.fromString
                        >> Result.map Dec.succeed
                        >> Result.withDefault (Dec.fail "bad date")
                    )
            )
        )


decodeContent : Decoder Content
decodeContent =
    Dec.field "content_type" Dec.string
        |> Dec.andThen
            (\str ->
                case str of
                    "Note" ->
                        Dec.succeed Note

                    "SN|Component" ->
                        Dec.succeed SnComponent

                    "SN|UserPreferences" ->
                        Dec.succeed SnUserPrefs

                    a ->
                        Dec.fail <| "Failed to decode Content Type: " ++ a
            )


type Content
    = Note
    | SnComponent
    | SnUserPrefs


log : a -> Cmd msg
log =
    Debug.log "!" >> always Cmd.none


main : Program (Maybe String) Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , subscriptions =
            \_ ->
                Window.resizes WindowSize
        , update =
            \msg model ->
                case msg of
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
                            (Dec.map2 Params (Dec.field "pw_cost" Dec.int) (Dec.field "pw_salt" Dec.string))
                            |> Http.toTask
                            |> Task.andThen
                                (\{ cost, salt } ->
                                    Http.post
                                        "/crypto/key"
                                        (Enc.object
                                            [ ( "salt", Enc.string salt )
                                            , ( "password", Enc.string model.password )
                                            , ( "cost", Enc.int cost )
                                            ]
                                            |> Http.jsonBody
                                        )
                                        Dec.string
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
                                        (Enc.object
                                            [ ( "email", Enc.string model.username )
                                            , ( "password", Enc.string pw )
                                            ]
                                            |> Http.jsonBody
                                        )
                                        (Dec.field "token" Dec.string
                                            |> Dec.map
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
                                    [ sync auth.token
                                    , Enc.object
                                        [ ( "token", Enc.string auth.token )
                                        , ( "authKey", Enc.string auth.authKey )
                                        , ( "encryptionKey", Enc.string auth.encryptionKey )
                                        ]
                                        |> Enc.encode 0
                                        |> Ports.saveAuth
                                    ]
                                )

                            Err err ->
                                ( model, log err )

                    NotesCb res ->
                        case res of
                            Ok data ->
                                let
                                    notes =
                                        data
                                            |> List.map
                                                (Dec.decodeString (Dec.field "title" Dec.string))
                                            |> List.filterMap Result.toMaybe
                                in
                                ( { model | notes = notes }, Cmd.none )

                            Err err ->
                                ( model, log err )

                    ItemsCb res ->
                        case res of
                            Ok items ->
                                case model.auth of
                                    Just { authKey, encryptionKey } ->
                                        ( model
                                        , items
                                            |> filterMap2
                                                .encItemKey
                                                .content
                                                (\encItemKey content ->
                                                    decrypt authKey encryptionKey encItemKey
                                                        |> Task.andThen
                                                            (\itemKey ->
                                                                let
                                                                    len =
                                                                        String.length itemKey // 2

                                                                    itemEncryptionKey =
                                                                        String.left len itemKey

                                                                    itemAuthKey =
                                                                        String.right len itemKey
                                                                in
                                                                decrypt itemAuthKey itemEncryptionKey content
                                                            )
                                                )
                                            |> Task.sequence
                                            |> Task.attempt NotesCb
                                        )

                                    Nothing ->
                                        ( { model | items = items }
                                        , log "no auth"
                                        )

                            Err err ->
                                ( model, log err )

                    UrlChange _ ->
                        ( model, Cmd.none )

                    WindowSize _ ->
                        ( model, Cmd.none )
        , view =
            \model ->
                layoutWith
                    { options =
                        [ noHover
                        , Element.focusStyle
                            { borderColor = Nothing
                            , backgroundColor = Nothing
                            , shadow = Nothing
                            }
                        ]
                    }
                    [ width fill ]
                <|
                    el [ centerY ] <|
                        column [ spacing 15 ]
                            [ Input.username [ Border.color Color.blue ]
                                { onChange = Just UpdateUsername
                                , placeholder = Just <| Input.placeholder [] <| text "username"
                                , text = model.username
                                , label = Input.labelLeft [] empty
                                }
                            , Input.currentPassword [ Border.color Color.blue ]
                                { onChange = Just UpdatePassword
                                , placeholder = Just <| Input.placeholder [] <| text "password"
                                , text = model.password
                                , label = Input.labelLeft [] empty
                                , show = False
                                }
                            , Input.button [ centerX ]
                                { onPress = Just Submit
                                , label = el [] <| text "submit"
                                }
                            , column []
                                (model.notes
                                    |> List.map (text >> el [])
                                )

                            --(model.items
                            --|> List.map
                            --(\item ->
                            --column [ width shrink ]
                            --[ el [] <| text item.uuid
                            --, paragraph [ width <| px 20 ] [ text <| Maybe.withDefault "!" item.encItemKey ]
                            --]
                            --)
                            --)
                            ]
        }



--{ pw_salt = "e5b690794e66a9ce8b13410bfedc78edc76f7848"
--, pw_cost = 5000
--, version = "002"
--, pw_func = "pbkdf2"
--, pw_alg = "sha512"
--, pw_key_size = 512
--}


sync : String -> Cmd Msg
sync token =
    Http.request
        { method = "POST"
        , url = "https://sync.standardnotes.org/items/sync"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , body =
            Enc.object
                [ ( "items", Enc.list (List.map Enc.string []) )

                --, ( "saved_items", Enc.list (List.map Enc.string []) )
                --, ( "unsaved_items", Enc.list (List.map Enc.string []) )
                , ( "sync_token", Enc.null )
                ]
                |> Http.jsonBody
        , expect = Http.expectJson (Dec.field "retrieved_items" (Dec.list decodeItem))
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ItemsCb


init : Maybe String -> Location -> ( Model, Cmd Msg )
init maybeData _ =
    maybeData
        |> Result.fromMaybe "No Auth Provided"
        |> Result.andThen (Dec.decodeString decodeAuth)
        |> useResult
            (\err ->
                ( emptyModel, log err )
            )
            (\auth ->
                ( { emptyModel | auth = Just auth }
                , sync auth.token
                )
            )


useResult : (e -> b) -> (a -> b) -> Result e a -> b
useResult fn1 fn2 result =
    case result of
        Ok a ->
            fn2 a

        Err e ->
            fn1 e


emptyModel : Model
emptyModel =
    { password = ""
    , username = ""
    , auth = Nothing
    , items = []
    , notes = []
    }


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


decrypt : String -> String -> String -> Task Http.Error String
decrypt authKey encryptionKey txt =
    Http.post
        "/crypto/decrypt"
        (Enc.object
            [ ( "text", Enc.string txt )
            , ( "authKey", Enc.string authKey )
            , ( "encryptionKey", Enc.string encryptionKey )
            ]
            |> Http.jsonBody
        )
        Dec.string
        |> Http.toTask


filterMap2 : (a -> Maybe b) -> (a -> Maybe c) -> (b -> c -> d) -> List a -> List d
filterMap2 fn1 fn2 fn3 =
    List.filterMap
        (\x ->
            Maybe.map2 fn3 (fn1 x) (fn2 x)
        )
