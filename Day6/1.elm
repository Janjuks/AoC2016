module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Day6.Input exposing (puzzleInput, exampleInput)


main : Html a
main =
    getPw 0 "" |> text


msgArr : Array (Array String)
msgArr =
    let
        rows =
            lines puzzleInput
    in
        rows
            |> List.map (\str -> Array.fromList <| String.split "" str)
            |> Array.fromList


getPwChar : Int -> Int -> Dict String Int -> String
getPwChar col row dict =
    if row < Array.length msgArr then
        let
            char =
                msgArr
                    |> Array.get row
                    |> Maybe.withDefault (Array.empty)
                    |> Array.get col
                    |> Maybe.withDefault ""

            newDict =
                Dict.update char (\v -> Just <| (Maybe.withDefault 0 v) + 1) dict
        in
            getPwChar col (row + 1) newDict
    else
        dict
            |> Dict.toList
            |> List.sortBy (\( k, v ) -> -v)
            |> List.unzip
            |> Tuple.first
            |> List.head
            |> Maybe.withDefault ""


getPw : Int -> String -> String
getPw col currentPw =
    if col < 8 then
        let
            newChar =
                getPwChar col 0 Dict.empty
        in
            getPw (col + 1) (currentPw ++ newChar)
    else
        currentPw
