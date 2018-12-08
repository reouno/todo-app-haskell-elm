module Utils.Events exposing (..)

import Json.Decode as JD
import Msgs exposing (Msg)

isEnter : Msg -> number -> JD.Decoder Msg
isEnter msg code =
    if code == 13 then
        JD.succeed msg
    else
        JD.fail "not Enter"
