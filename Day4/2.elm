module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)
import Dict exposing (..)
import Char exposing (..)
import Day4.Input exposing (puzzleInput)


main : Html a
main =
    text <| toString res


res =
    puzzleInput
        |> lines
        |> List.map getRoom
        |> Debug.log "rooms"
        |> List.filter (\room -> room.realChecksum == room.givenChecksum)


regexes : { checksum : Regex, id : Regex, name : Regex }
regexes =
    { name = regex "^([\\D]+)"
    , id = regex "([\\d]+)"
    , checksum = regex "\\[(\\w+)\\]"
    }


swapChar : Int -> Char -> Char
swapChar amount char =
    if char /= '-' then
        char
            |> toCode
            |> flip (-) 97
            |> (+) amount
            |> flip rem 26
            |> (+) 97
            |> fromCode
    else
        ' '


type alias Room =
    { name : String
    , decodedName : String
    , id : Int
    , realChecksum : String
    , givenChecksum : String
    }


getChecksum : List String -> Dict String Int -> String
getChecksum chars checksumDict =
    let
        char =
            List.head chars

        newDict =
            case char of
                Nothing ->
                    checksumDict

                Just val ->
                    Dict.update val (\v -> Just <| (Maybe.withDefault 0 v) + 1) checksumDict
    in
        case char of
            Nothing ->
                newDict
                    |> Dict.toList
                    |> List.sortWith checkSumComparison
                    |> List.unzip
                    |> Tuple.first
                    |> List.take 5
                    |> concat

            Just val ->
                getChecksum (List.drop 1 chars) newDict



--sort first by highest char repeat count and if equals then alphabetically


checkSumComparison : ( comparable, comparable1 ) -> ( comparable, comparable1 ) -> Order
checkSumComparison ( k1, v1 ) ( k2, v2 ) =
    case compare v1 v2 of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            compare k1 k2


getRoom : String -> Room
getRoom codedRoom =
    let
        name =
            codedRoom
                |> Regex.find All regexes.name
                |> List.head
                |> Maybe.map .match
                |> Maybe.withDefault ""

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
                |> Maybe.map .submatches
                |> Maybe.withDefault []
                |> List.head
                |> Maybe.withDefault Maybe.Nothing
                |> Maybe.withDefault ""

        realChecksum =
            getChecksum (String.split "" <| Regex.replace All (regex "-") (\_ -> "") name) Dict.empty

        swapCharByAmount =
            swapChar id

        decodedName =
            name
                |> String.toList
                |> List.map swapCharByAmount
                |> String.fromList
    in
        { name = name
        , decodedName = decodedName
        , id = id
        , realChecksum = realChecksum
        , givenChecksum = checksum
        }
