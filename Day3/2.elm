module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Array.Hamt as Array
import Day3.Input exposing (puzzleInput)


main : Html a
main =
    text (toString trianglesCount)


trianglesCount : Int
trianglesCount =
    let
        boolTriangles =
            List.map isTriangle (getTrianglesList sides [])

        trueTriangles =
            List.filter (\triangle -> triangle) boolTriangles
    in
        List.length trueTriangles


getTrianglesList : List Int -> List (List Int) -> List (List Int)
getTrianglesList sides currentList =
    let
        nineSides =
            Debug.log "9sides"
                List.take
                9
                sides

        newCurrentList =
            if List.length nineSides == 0 then
                currentList
            else
                List.append currentList (getTriangles nineSides)
    in
        if List.length nineSides == 0 then
            newCurrentList
        else
            getTrianglesList (List.drop 9 sides) newCurrentList


getTriangle : Array.Array Int -> Int -> List Int
getTriangle sides n =
    let
        sideA =
            Maybe.withDefault -1 (Array.get n sides)

        sideB =
            Maybe.withDefault -1 (Array.get (n + 3) sides)

        sideC =
            Maybe.withDefault -1 (Array.get (n + 6) sides)
    in
        [ sideA, sideB, sideC ]


getTriangles : List Int -> List (List Int)
getTriangles listSides =
    let
        sides =
            if List.length listSides /= 9 then
                Debug.crash "shit happened"
            else
                Array.fromList listSides

        triangle1 =
            getTriangle sides 0

        triangle2 =
            getTriangle sides 1

        triangle3 =
            getTriangle sides 2
    in
        [ triangle1, triangle2, triangle3 ]


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


sides : List Int
sides =
    List.map (\s -> Result.withDefault -1 (toInt s)) (words puzzleInput)
