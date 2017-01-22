module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)
import Regex exposing (..)
import Array.Hamt as Array
import Day8.Input exposing (puzzleInput, exampleInput)


main : Html a
main =
    -- drawRect screen (Width 3) (Height 2)
    --     |> screenView
    screen
        |> flip (flip drawRect (Width 3)) (Height 2)
        |> screenView


type Screen
    = Screen (Array.Array (Array.Array Bool))


screen : Screen
screen =
    Screen (Array.repeat 6 (Array.repeat 50 False))


drawRect : Screen -> Width -> Height -> Screen
drawRect (Screen screen) (Width w) (Height h) =
    Screen
        (Array.indexedMap
            (\y row ->
                if y < h then
                    Array.indexedMap
                        (\x col ->
                            if x < w then
                                True
                            else
                                col
                        )
                        row
                else
                    row
            )
            screen
        )


type Width
    = Width Int


type Height
    = Height Int


type Row
    = Row Int


type Column
    = Column Int


type Amount
    = Amount Int


type Instruction
    = Rect ( Width, Height )
    | RotateRow ( Row, Amount )
    | RotateCol ( Column, Amount )


screenView : Screen -> Html msg
screenView (Screen screen) =
    screen
        |> Array.toList
        |> List.map rowView
        |> table []


rowView : Array.Array Bool -> Html msg
rowView row =
    row
        |> Array.toList
        |> List.map colView
        |> tr []


colView : Bool -> Html msg
colView col =
    if col then
        td [] [ text "#" ]
    else
        td [] [ text "." ]
