module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)
import Day9.Input exposing (exampleInput, puzzleInput)


main : Html a
main =
    exampleInput
        |> decodeFile
        |> text



-- let
--     res =
--         decodeFile exampleInput ""
--
--     resLi =
--         li [] [ text res ]
--
--     resLengthLi =
--         li [] [ res |> length |> toString |> text ]
-- in
--     ul [] (resLi :: [ resLengthLi ])


decodeFile : String -> String
decodeFile file =
    let
        allMarkers =
            file
                |> Debug.log "file"
                |> Regex.find All markerRgx
                |> List.map getMarker

        topLevelMarkers =
            allMarkers
                |> List.filter (isTopLevelMarker allMarkers)
    in
        if List.isEmpty topLevelMarkers then
            file
                |> Debug.log "decodedFile"
        else
            topLevelMarkers
                |> List.map (repeatSliceDecode file topLevelMarkers)
                |> concat
                |> Debug.log "decodedFile"



-- List.length allMarkers
--     |> toString
--     |> flip (++) " "
--     |> flip (++) (topLevelMarkers |> List.length |> toString)


repeatSliceDecode : String -> List Marker -> Marker -> String
repeatSliceDecode file markers marker =
    sliceByMarker file markers marker
        |> Debug.log "slicedByMarker"
        |> decodeFile
        -- |> Debug.log "decodedAfterSlice"
        |>
            repeat marker.repeatCount



-- |> Debug.log "repeated"


sliceByMarker : String -> List Marker -> Marker -> String
sliceByMarker file markers marker =
    let
        --slice till next marker or end of file
        endIndex =
            markers
                |> List.filter (\m -> m.startIndex > marker.startIndex)
                |> List.head
                |> Maybe.withDefault
                    { charCount = 0
                    , repeatCount = 0
                    , startIndex = length file
                    , endIndex = 0
                    }
                |> .startIndex
    in
        file
            |> slice (marker.endIndex + 1) endIndex


isTopLevelMarker : List Marker -> Marker -> Bool
isTopLevelMarker allMarkers marker =
    --True if none of previous markers' range overlaps current marker's index
    allMarkers
        |> List.filter (\m -> m.startIndex < marker.startIndex)
        |> List.any (\m -> m.endIndex + -1 + marker.charCount > marker.startIndex)
        |> not


getMarker : Match -> Marker
getMarker marker =
    let
        charAndRepeatCount =
            marker.match
                |> dropLeft 1
                |> dropRight 1
                |> String.split "x"

        charCount =
            charAndRepeatCount
                |> List.head
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault -1

        repeatCount =
            charAndRepeatCount
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault -1

        endIndex =
            marker.match
                |> length
                |> (+) -1
                |> (+) marker.index
    in
        { charCount = charCount
        , repeatCount = repeatCount
        , startIndex = marker.index
        , endIndex = endIndex
        }


markerRgx : Regex
markerRgx =
    regex "\\(\\d+x\\d+\\)"


type alias Marker =
    { charCount : Int
    , repeatCount : Int
    , startIndex : Int
    , endIndex : Int
    }
