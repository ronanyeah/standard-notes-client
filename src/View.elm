module View exposing (view)

import Color
import Dict
import Element exposing (centerX, centerY, column, el, empty, fill, layoutWith, link, noHover, spacing, text, width)
import Element.Border as Border
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Types exposing (..)


view : Model -> Html Msg
view model =
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
        case model.view of
            LoginView ->
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
                        ]

            NotesView ->
                column []
                    (model.notes
                        |> Dict.values
                        |> List.map
                            (\{ uuid, title } ->
                                button []
                                    { onPress = Just <| GoTo <| NoteRoute uuid
                                    , label = el [] <| text title
                                    }
                            )
                    )

            TagsView ->
                column []
                    (model.tags
                        |> Dict.values
                        |> List.map
                            (\{ uuid, title } ->
                                button []
                                    { onPress = Just <| GoTo <| TagRoute uuid
                                    , label = el [] <| text title
                                    }
                            )
                    )

            NoteView note ->
                case model.auth of
                    Just auth ->
                        column []
                            [ Input.text []
                                { onChange = Just UpdateTitle
                                , text = model.title
                                , placeholder = Nothing
                                , label = Input.labelLeft [] empty
                                }
                            , Input.multiline []
                                { onChange = Just UpdateText
                                , text = model.text
                                , placeholder = Nothing
                                , label = Input.labelLeft [] empty
                                , spellcheck = True
                                }
                            , button []
                                { onPress = Just <| SaveNote auth note
                                , label = el [] <| text "SAVE"
                                }
                            , button []
                                { onPress = Just <| GoTo NotesRoute
                                , label = el [] <| text "CANCEL"
                                }
                            ]

                    Nothing ->
                        button []
                            { onPress = Just <| GoTo LoginRoute
                            , label = el [] <| text "Login"
                            }

            LoadingView ->
                el [ centerX, centerY ] <| text "Loading..."

            ServiceWorkerFailView ->
                link []
                    { url = "/"
                    , label = text "Service worker fail. Reload?"
                    }
