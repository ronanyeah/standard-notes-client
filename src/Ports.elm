port module Ports exposing (..)


port log : String -> Cmd msg


port saveAuth : String -> Cmd msg


port clearAuth : () -> Cmd msg
