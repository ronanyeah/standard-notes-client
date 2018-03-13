module Router exposing (router)

import Navigation exposing (Location)
import Types exposing (..)
import UrlParser exposing ((</>), Parser, map, oneOf, parsePath, s, string)


routes : List (Parser (Route -> a) a)
routes =
    [ map LoginRoute (s "login")
    , map NotesRoute (s "notes")
    , map NoteRoute (s "notes" </> string)
    ]


router : Location -> Route
router =
    parsePath (oneOf routes)
        >> Maybe.withDefault NotFoundRoute
