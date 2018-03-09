port module Ports exposing (..)

import Json.Decode exposing (Value)


port saveAuth : String -> Cmd msg
