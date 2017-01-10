module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)
import Day7.Input exposing (puzzleInput, exampleInput)


main : Html a
main =
    res |> toString |> text


res : Int
res =
    puzzleInput
        |> lines
        |> List.filter validIP
        |> List.length


validIP : String -> Bool
validIP ip =
    let
        aba =
            ip
                |> Regex.find (AtMost 1) abaRgx
                |> List.concatMap .submatches
                |> List.head
                |> Maybe.andThen identity
                |> Maybe.withDefault ""
                |> Debug.log "aba"

        bab =
            if aba == "" then
                ""
            else
                aba
                    |> String.slice 0 1
                    |> babRgx (String.slice 1 2 aba)
                    |> flip (Regex.find (AtMost 1)) ip
                    |> List.head
                    |> Maybe.map .match
                    |> Maybe.withDefault ""
    in
        bab /= ""


abaRgx : Regex
abaRgx =
    regex "(?!\\[)\\w*((\\w)(?!\\2)(\\w)\\2)\\w*(?!\\])"


babRgx : String -> String -> Regex
babRgx b a =
    String.join "" [ "\\[\\w*", b, a, b, "\\w*\\]" ] |> Debug.log "babRgx" |> regex
