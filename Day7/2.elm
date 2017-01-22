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
        abas =
            ip
                |> Regex.replace All (regex "\\[\\w*\\]") (\_ -> " ")
                |> findAbas []
    in
        abas
            |> List.any (\v -> babExists ip v)


findAbas : List String -> String -> List String
findAbas abas ip =
    let
        ( aba, abaIndex ) =
            ip
                |> Regex.find (AtMost 1) abaRgx
                |> List.map (\v -> ( v.match, v.index ))
                |> List.head
                |> Maybe.withDefault ( "", -1 )
    in
        if abaIndex == -1 then
            abas
        else
            findAbas (aba :: abas) (dropLeft (abaIndex + 1) ip)


babExists : String -> String -> Bool
babExists ip aba =
    aba
        |> String.slice 0 1
        |> babRgx (String.slice 1 2 aba)
        |> flip Regex.contains ip


abaRgx : Regex
abaRgx =
    regex "((\\w)(?!\\2)(\\w)\\2)"


babRgx : String -> String -> Regex
babRgx b a =
    String.join "" [ "\\[\\w*", b, a, b, "\\w*\\]" ]
        |> regex
