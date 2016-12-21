module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Day3.Input exposing (puzzleInput)


main : Html a
main =
    text (toString <| trianglesCount)


trianglesCount : Int
trianglesCount =
    let
        boolTriangles =
            List.map isTriangle maybeTriangles

        trueTriangles =
            List.filter (\triangle -> triangle) boolTriangles
    in
        List.length trueTriangles


isTriangle : List Int -> Bool
isTriangle sides =
    let
        max =
            Maybe.withDefault -1 (List.maximum sides)
    in
        if List.sum sides - max > max then
            True
        else
            False


maybeTriangles : List (List Int)
maybeTriangles =
    List.map
        (List.map
            (\sideLength ->
                Result.withDefault -1 (toInt sideLength)
            )
        )
        stringsOfNumbers


stringsOfNumbers : List (List String)
stringsOfNumbers =
    List.map (words) <| lines puzzleInput
