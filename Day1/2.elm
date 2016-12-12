module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Tuple exposing (..)
import Array exposing (..)


main : Html a
main =
    text (toString result ++ toString (abs (first result.coords) + abs (second result.coords)))


type alias Model =
    { direction : Int
    , coords : ( Int, Int )
    , targetReached : Bool
    , visitedCoords : List ( Int, Int )
    }


model : Model
model =
    Model 90 ( 0, 0 ) False []


result : Model
result =
    getResult model 0 moves


input : String
input =
    "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"


moves : Array String
moves =
    Array.fromList (split ", " input)


getDirection : Int -> String -> ( Int, Int, Int )
getDirection direction move =
    let
        change =
            case (left 1 move) of
                "R" ->
                    -90

                "L" ->
                    90

                _ ->
                    Debug.crash "error"

        dir =
            (abs direction + change) % 360
    in
        case dir of
            0 ->
                ( 1, 0, dir )

            90 ->
                ( 0, 1, dir )

            180 ->
                ( -1, 0, dir )

            270 ->
                ( 0, -1, dir )

            _ ->
                ( 0, 0, dir )


move : Model -> String -> Model
move model move =
    let
        ( x, y, newDir ) =
            getDirection model.direction move

        blocks =
            Result.withDefault 0 (toInt (dropLeft 1 move))

        newModel =
            step model blocks ( x, y )
    in
        { newModel
            | direction = newDir
        }


step : Model -> Int -> ( Int, Int ) -> Model
step model steps ( x, y ) =
    let
        newCoords =
            ( first model.coords + x, second model.coords + y )

        newModel =
            { model
                | coords = newCoords
                , visitedCoords = newCoords :: model.visitedCoords
            }
    in
        if steps < 1 then
            model
        else if coordsVisited (List.drop 1 newModel.visitedCoords) newCoords then
            { newModel
                | targetReached = True
            }
        else
            step newModel (steps - 1) ( x, y )


getResult : Model -> Int -> Array String -> Model
getResult model index moves =
    let
        currentMove =
            Array.get index moves

        newModel =
            if currentMove == Maybe.Nothing then
                Debug.crash "error"
            else
                (move model (Maybe.withDefault "" currentMove))
    in
        if (newModel.targetReached) then
            newModel
        else
            getResult newModel (index + 1) moves


coordsVisited : List ( Int, Int ) -> ( Int, Int ) -> Bool
coordsVisited visitedCoords coords =
    if List.member coords (Debug.log "visitedCoords" visitedCoords) then
        Debug.log "coords found!!"
            True
    else
        False
