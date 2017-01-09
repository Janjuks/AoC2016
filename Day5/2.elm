module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import MD5 exposing (..)
import Array exposing (..)
import Day5.Input exposing (puzzleInput)


main : Html a
main =
    password puzzleInput 0 (Array.repeat 8 "")
        |> Array.toList
        |> String.concat
        |> text


password : String -> number -> Array String -> Array String
password id index progress =
    let
        hash =
            hex (id ++ (toString index))

        newProgress =
            updatePw hash progress
    in
        if pwFound newProgress then
            newProgress
        else
            password id (index + 1) newProgress


updatePw : String -> Array String -> Array String
updatePw hash currentPw =
    if startsWith "00000" hash then
        let
            pos =
                Result.withDefault -1 <| String.toInt <| String.slice 5 6 hash

            val =
                if 0 <= pos && pos <= 7 then
                    String.slice 6 7 hash
                else
                    ""

            arrayVal =
                if val /= "" then
                    case Array.get pos currentPw of
                        Just v ->
                            v

                        Nothing ->
                            Debug.crash "this shouldn't happen"
                else
                    "notHashChar"

            newPw =
                if arrayVal == "" then
                    Array.set pos val currentPw
                else
                    currentPw
        in
            newPw
    else
        currentPw


pwFound : Array String -> Bool
pwFound pw =
    pw
        |> Array.toList
        |> List.all (\c -> c /= "")
