module Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Json.Decode
import Navigation exposing (Location)
import Window exposing (Size)


type View
    = LoginView
    | NoteView Note
    | NotesView
    | TagsView
    | LoadingView
    | ServiceWorkerFailView


type Route
    = LoginRoute
    | NoteRoute String
    | NotesRoute
    | NotFoundRoute
    | TagsRoute
    | TagRoute String


type
    Content
    --| SnComponent
    --| SnUserPrefs
    = NoteContent Note
    | TagContent Tag
    | Other Item


type Msg
    = UrlChange Location
    | GoTo Route
    | ServiceWorkerCheck Location (Result Http.Error ())
    | SignInCb (Result Http.Error Auth)
    | SyncCb Auth (Result Http.Error Sync)
    | UpdateText String
    | UpdateTitle String
    | SaveNote Auth Note
    | WindowSize Size
    | UpdatePassword String
    | UpdateUsername String
    | UpdateNotes (Result Http.Error (Dict String Note))
    | Submit
    | ValueCb (Result Http.Error Json.Decode.Value)


type alias Model =
    { password : String
    , username : String
    , auth : Maybe Auth
    , syncToken : Maybe String
    , view : View
    , title : String
    , text : String
    , notes : Dict String Note
    , tags : Dict String Tag
    }


type alias Params =
    { cost : Int
    , salt : String
    }


type alias Item =
    { uuid : String
    , content : Maybe String
    , contentType : String
    , encItemKey : Maybe String
    , deleted : Bool
    , createdAt : Date
    , updatedAt : Date
    }


type alias Note =
    { uuid : String
    , title : String
    , text : String
    , createdAt : Date
    , updatedAt : Date
    }


type alias Tag =
    { uuid : String
    , title : String
    , createdAt : Date
    , updatedAt : Date
    }


type alias Auth =
    { token : String
    , encryptionKey : String
    , authKey : String
    }


type alias Sync =
    { retrievedItems : List Item
    , savedItems : List Item
    , unsavedItems : List Item
    , syncToken : Maybe String
    , cursorToken : Maybe String
    }
