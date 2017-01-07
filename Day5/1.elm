module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import MD5 exposing (..)
import Day5.Input exposing (puzzleInput)


main : Html a
main =
    text <| password puzzleInput 0 ""


password : String -> number -> String -> String
password id index progress =
    let
        hash =
            hex (id ++ (toString index))

        newProgress =
            if startsWith "00000" hash then
                progress
                    ++ (String.slice 5 6 hash)
            else
                progress
    in
        if String.length newProgress == 8 then
            newProgress
        else
            password id (index + 1) newProgress
