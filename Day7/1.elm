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
        |> Regex.contains regexes.abba
        |> (&&) (not (Regex.contains regexes.hypernetAbba ip))


regexes : { hypernetAbba : Regex, abba : Regex }
regexes =
    { abba = regex "(\\w)(?!\\1)(\\w)\\2\\1(?!\\w*\\])"
    , hypernetAbba = regex "(\\w)(?!\\1)(\\w)\\2\\1(?=\\w*\\])"
    }
