module Utils exposing (..)

import Dict exposing (Dict)
import Http
import Json
import Navigation
import Ports
import Types exposing (..)


goTo : Route -> Cmd Msg
goTo route =
    case route of
        NotesRoute ->
            Navigation.newUrl "/notes"

        NoteRoute id ->
            Navigation.newUrl <| "/notes/" ++ id

        LoginRoute ->
            Navigation.newUrl "/login"

        NotFoundRoute ->
            Navigation.newUrl "/login"

        TagsRoute ->
            Navigation.newUrl "/tags"

        TagRoute id ->
            Navigation.newUrl <| "/tags/" ++ id


log : a -> Cmd msg
log =
    toString >> Ports.log


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
    , syncToken = Nothing
    , view = LoadingView
    , title = ""
    , text = ""
    , notes = Dict.empty
    , tags = Dict.empty
    }


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default fn =
    Maybe.map fn
        >> Maybe.withDefault default


sync : List Item -> Maybe String -> String -> Http.Request Sync
sync items syncToken token =
    Http.request
        { method = "POST"
        , url = "https://sync.standardnotes.org/items/sync"
        , headers = [ Http.header "Authorization" <| "Bearer " ++ token ]
        , body =
            Json.encodeSync items syncToken
                |> Http.jsonBody
        , expect = Http.expectJson Json.decodeSync
        , timeout = Nothing
        , withCredentials = False
        }


isCode : Int -> Http.Error -> Bool
isCode code err =
    case err of
        Http.BadStatus { status } ->
            status.code == code

        _ ->
            False


itemDict : List { r | uuid : String } -> Dict String { r | uuid : String }
itemDict =
    List.map
        (\r ->
            ( r.uuid, r )
        )
        >> Dict.fromList
