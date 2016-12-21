module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)


main : Html a
main =
    text (toString <| maybeTriangles)


maybeTriangles : List ( Int, Int, Int )
maybeTriangles =
    List.map
        (\arr ->
            let
                sideA =
                    Maybe.withDefault "-1" <| List.head arr

                sideB =
                    Maybe.withDefault "-1" <| List.head <| List.drop 1 arr

                sideC =
                    Maybe.withDefault "-1" <| List.head <| List.drop 2 arr
            in
                ( sideA, sideB, sideC )
        )
        stringsOfNumbers


stringsOfNumbers : List (List String)
stringsOfNumbers =
    List.map (words) <| lines puzzleInput
