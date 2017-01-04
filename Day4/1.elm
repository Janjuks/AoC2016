module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)
import Day4.Input exposing (puzzleInput)


main : Html a
main =
    text "a"



{- name
   ^([\D]+)
-}
{- id
   ([\d]+)
-}
{- checksum
   \[(\w+)\]
-}


regexes : { checksum : Regex, id : Regex, name : Regex }
regexes =
    { name = regex "^([\\D]+)"
    , id = regex "([\\d]+)"
    , checksum = regex "\\[(\\w+)\\]"
    }


type alias Room =
    { name : String
    , id : Int
    , realChecksum : String
    , givenChecksum : String
    }


getRooms : List String -> List Room
getRooms codedRooms =
    []


getRoom : String -> Room
getRoom codedRoom =
    let
        name =
            codedRoom
                |> Regex.find All regexes.name
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""
                |> Regex.replace All (regex "-") (\_ -> "")

        id =
            codedRoom
                |> Regex.find All regexes.id
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault 0

        checksum =
            codedRoom
                |> Regex.find All regexes.checksum
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""
    in
        { name = name, id = id, realChecksum = "a", givenChecksum = checksum }



--lines puzzleInput
