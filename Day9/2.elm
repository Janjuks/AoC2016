module Main exposing (..)

import Html exposing (..)
import String exposing (..)
import Day9.Input exposing (exampleInput, puzzleInput)


main : Html a
main =
    exampleInput
        |> flip decodeFile ""
        |> length
        |> toString
        |> text


decodeFile : String -> String -> String
decodeFile file decodedFile =
    let
        openingBracketIndex =
            file
                |> indexes "("
                |> List.head
                |> Maybe.withDefault -1

        closingBracketIndex =
            file
                |> indexes ")"
                |> List.head
                |> Maybe.withDefault -1

        ( charCount, repeatCount ) =
            if openingBracketIndex /= -1 && closingBracketIndex /= -1 then
                file
                    |> slice (openingBracketIndex + 1) closingBracketIndex
                    |> getMarker
            else
                ( -1, -1 )

        newDecodedFile =
            if openingBracketIndex /= -1 && closingBracketIndex /= -1 then
                file
                    |> slice 0 openingBracketIndex
                    |> flip append (getRepeatedPhrase file (closingBracketIndex + 1) charCount repeatCount)
                    |> append decodedFile
            else
                decodedFile
    in
        if openingBracketIndex /= -1 && closingBracketIndex /= -1 then
            decodeFile (slice (closingBracketIndex + 1 + charCount) (length file) file) newDecodedFile
        else
            newDecodedFile ++ file


getMarker : String -> ( Int, Int )
getMarker marker =
    let
        charsAndRepeat =
            marker
                |> split "x"
    in
        ( charsAndRepeat
            |> List.head
            |> Maybe.withDefault ""
            |> toInt
            |> Result.withDefault -1
        , charsAndRepeat
            |> List.drop 1
            |> List.head
            |> Maybe.withDefault ""
            |> toInt
            |> Result.withDefault -1
        )


getRepeatedPhrase : String -> Int -> Int -> Int -> String
getRepeatedPhrase file startIndex charCount_ repeatCount_ =
    let
        slicedFile =
            file
                |> slice startIndex (startIndex + charCount_)

        openingBracketIndex =
            slicedFile
                |> indexes "("
                |> List.head
                |> Maybe.withDefault -1

        closingBracketIndex =
            slicedFile
                |> indexes ")"
                |> List.head
                |> Maybe.withDefault -1

        ( charCount, repeatCount ) =
            if openingBracketIndex /= -1 && closingBracketIndex /= -1 then
                slicedFile
                    |> slice (openingBracketIndex + 1) closingBracketIndex
                    |> getMarker
            else
                ( -1, -1 )
    in
        if openingBracketIndex /= -1 && closingBracketIndex /= -1 then
            getRepeatedPhrase slicedFile (closingBracketIndex + 1) charCount repeatCount
                |> repeat repeatCount_
        else
            repeat repeatCount_ slicedFile
