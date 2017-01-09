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
    ip
        |> Regex.contains regexes.ssl1
        |> (||) (Regex.contains regexes.ssl2 ip)


regexes : { ssl1 : Regex, ssl2 : Regex }
regexes =
    { ssl1 = regex "\\[\\w*(\\w)(?!\\1)(\\w)\\1\\w*\\]\\w*\\2\\1\\2"
    , ssl2 = regex "(\\w)(?!\\1)(\\w)\\1\\w*\\[\\w*\\2\\1\\2\\w*\\]"
    }
