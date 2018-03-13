module Json exposing (..)

import Date
import Date.Extra exposing (toUtcIsoString)
import Json.Decode as Dec exposing (Decoder, Value)
import Json.Encode as Enc
import Types exposing (..)


decodeNote : Item -> Decoder Note
decodeNote { createdAt, updatedAt, uuid } =
    Dec.map5 Note
        (Dec.succeed uuid)
        (Dec.field "title" Dec.string)
        (Dec.field "text" Dec.string)
        (Dec.succeed createdAt)
        (Dec.succeed updatedAt)


decodeTag : Item -> Decoder Tag
decodeTag { createdAt, updatedAt, uuid } =
    Dec.map4 Tag
        (Dec.succeed uuid)
        (Dec.field "title" Dec.string)
        (Dec.succeed createdAt)
        (Dec.succeed updatedAt)


decodeParams : Decoder Params
decodeParams =
    Dec.map2 Params
        (Dec.field "pw_cost" Dec.int)
        (Dec.field "pw_salt" Dec.string)


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
        (Dec.field "content_type" Dec.string)
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


decodeContent_ : Decoder Content
decodeContent_ =
    Dec.field "content_type" Dec.string
        |> Dec.andThen
            (\str ->
                case str of
                    "Note" ->
                        --Dec.succeed Note
                        --Dec.field "content" (Dec.map Encrypted Dec.string)
                        Dec.fail "Note"

                    "Tag" ->
                        --Dec.succeed Tag
                        --Dec.field "content" (Dec.map Encrypted Dec.string)
                        Dec.fail "Tag"

                    "SN|Component" ->
                        --Dec.succeed SnComponent
                        Dec.fail "SN|Component"

                    "SN|UserPreferences" ->
                        --Dec.succeed SnUserPrefs
                        Dec.fail "SN|UserPreferences"

                    a ->
                        Dec.fail <| "Failed to decode Content Type: " ++ a
            )


decodeContent : Item -> Decoder Content
decodeContent item =
    Dec.oneOf
        [ Dec.map NoteContent (decodeNote item)
        , Dec.map TagContent (decodeTag item)
        , Dec.map Other <| Dec.succeed item
        ]


decodeSync : Decoder Sync
decodeSync =
    Dec.map5 Sync
        (Dec.field "retrieved_items" (Dec.list decodeItem))
        (Dec.field "saved_items" (Dec.list decodeItem))
        (Dec.field "unsaved" (Dec.list decodeItem))
        (Dec.field "sync_token" (Dec.nullable Dec.string))
        (Dec.field "cursor_token" (Dec.nullable Dec.string))


encodeSync : List Item -> Maybe String -> Value
encodeSync items token =
    Enc.object
        [ ( "items", Enc.list (List.map encodeItem items) )
        , ( "sync_token", encodeMaybeString token )
        ]


encodeMaybeString : Maybe String -> Value
encodeMaybeString =
    Maybe.map Enc.string
        >> Maybe.withDefault Enc.null


encodeItem : Item -> Value
encodeItem item =
    Enc.object
        [ ( "uuid", Enc.string item.uuid )
        , ( "content", encodeMaybeString item.content )
        , ( "content_type", Enc.string item.contentType )
        , ( "enc_item_key", encodeMaybeString item.encItemKey )
        , ( "deleted", Enc.bool item.deleted )
        , ( "auth_hash", Enc.null )
        , ( "created_at", Enc.string <| toUtcIsoString item.createdAt )
        , ( "updated_at", Enc.string <| toUtcIsoString item.updatedAt )
        ]
