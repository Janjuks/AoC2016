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
        |> Debug.log "validIPs?"
        |> List.length


validIP : String -> Bool
validIP ip =
    ip
        |> find (AtMost 1) regexes.abba
        |> List.isEmpty
        |> not
        |> (&&) (find (AtMost 1) regexes.hypernetAbba ip |> List.isEmpty)


regexes : { hypernetAbba : Regex, abba : Regex }
regexes =
    { abba = regex "(\\w)(?!\\1)(\\w)\\2\\1(?!\\w*\\])"
    , hypernetAbba = regex "(\\w)(?!\\1)(\\w)\\2\\1(?=\\w*\\])"
    }
