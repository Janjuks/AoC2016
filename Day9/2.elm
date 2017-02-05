module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Regex exposing (..)
import Day9.Input exposing (exampleInput, puzzleInput)

main : Html a
main =
      decodeFile puzzleInput
      |> toString
      |> text


decodeFile : String -> Int
decodeFile file =
    let
        allMarkers =
            file
                |> Regex.find All markerRgx
                |> List.map getMarker

        topLevelMarkers =
            allMarkers
                |> List.filter (isTopLevelMarker allMarkers)
    in
        if List.isEmpty topLevelMarkers then
            length file
        else
            topLevelMarkers
                |> List.map (repeatSliceDecode file topLevelMarkers)
                |> List.sum


repeatSliceDecode : String -> List Marker -> Marker -> Int
repeatSliceDecode file markers marker =
    let
        slice =
            sliceByMarker file markers marker
    in
        decodeFile slice.mid
          |> (*) marker.repeatCount
          |> (+) (length slice.prefix)
          |> (+) (length slice.suffix)


sliceByMarker : String -> List Marker -> Marker -> Slice
sliceByMarker file markers marker =
    let
        prefix =
            file
                |> slice prefixStartIndex marker.startIndex
                |> trim

        mid =
            file
                |> slice (marker.endIndex + 1) (marker.endIndex + marker.charCount + 1)
                |> trim

        suffix =
            file
                |> slice (marker.endIndex + marker.charCount + 1) suffixEndIndex
                |> trim

        --slice till next marker or end of file
        suffixEndIndex =
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

        previousMarker =
            markers
                |> List.filter (\m -> m.startIndex < marker.startIndex)
                |> List.reverse
                |> List.head

        prefixStartIndex =
            case previousMarker of
              --
                Nothing ->
                    0

                Just val ->
                  marker.startIndex
    in
        { prefix = prefix, mid = mid, suffix = suffix }


isTopLevelMarker : List Marker -> Marker -> Bool
isTopLevelMarker allMarkers marker =
    --True if none of previous markers' range overlaps current marker's index
    allMarkers
        |> List.filter (\m -> m.startIndex < marker.startIndex)
        |> List.any (\m -> m.endIndex + -1 + m.charCount >= marker.endIndex)
        |> not


getMarkers : String -> List Marker
getMarkers file =
    let
        allMarkers =
            file
                |> Regex.find All markerRgx
                |> List.map getMarker

        topLevelMarkers =
            allMarkers
                |> List.filter (isTopLevelMarker allMarkers)
    in
        topLevelMarkers


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


type alias Slice =
    { prefix : String
    , mid : String
    , suffix : String
    }
