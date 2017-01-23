module Main exposing (..)

import Html exposing (..)
import String exposing (..)


-- import Regex exposing (..)

import List exposing (..)


-- import Array.Hamt as Array

import Array exposing (..)
import Day8.Input exposing (puzzleInput, exampleInput)


--DATA


screenWidth : Int
screenWidth =
    50


screenHeight : Int
screenHeight =
    6


type Screen
    = Screen (Array.Array (Array.Array Bool))


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



--LOGIC


main : Html a
main =
    res



-- screen
--     |> flip (flip drawRect (Width 3)) (Height 2)
--     |> screenView


res : Html msg
res =
    execInstruction screen (List.take 28 instructions)
        |> screenView


screen : Screen
screen =
    Screen (Array.repeat screenHeight (Array.repeat screenWidth False))


execInstruction : Screen -> List Instruction -> Screen
execInstruction (Screen screen) instructions =
    let
        instrMaybe =
            List.head instructions

        newScreen =
            case instrMaybe of
                Just instr ->
                    case instr of
                        Rect ( Width w, Height h ) ->
                            drawRect (Screen screen) (Width w) (Height h)

                        RotateRow ( Row row, Amount amount ) ->
                            rotateRow (Screen screen) (Row row) (Amount amount)

                        RotateCol ( Column col, Amount amount ) ->
                            rotateColumn (Screen screen) (Column col) (Amount amount)

                Nothing ->
                    Screen screen
                        |> Debug.log "end of instructions"

        screenAsList =
            screen
                |> Array.toList
                |> List.map Array.toList
    in
        case instrMaybe of
            Just val ->
                execInstruction newScreen (List.drop 1 instructions)

            Nothing ->
                Screen screen


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


rotateColumn : Screen -> Column -> Amount -> Screen
rotateColumn (Screen screen) (Column col_) (Amount amount) =
    Screen
        (Array.indexedMap
            (\y row ->
                Array.indexedMap
                    (\x col ->
                        if x == col_ then
                            getColCell (Screen screen) (Row y) (Column x) (Amount amount)
                        else
                            col
                    )
                    row
            )
            screen
        )


getColCell : Screen -> Row -> Column -> Amount -> Bool
getColCell (Screen screen) (Row row) (Column col) (Amount amount) =
    screen
        |> Array.get row
        |> Maybe.withDefault Array.empty
        |> Array.get ((col - amount) % screenHeight)
        |> Maybe.withDefault False


rotateRow : Screen -> Row -> Amount -> Screen
rotateRow (Screen screen) (Row row_) (Amount amount) =
    Screen
        (Array.indexedMap
            (\y row ->
                if y == row_ then
                    row
                        |> Array.slice (screenWidth - amount) screenWidth
                        |> flip Array.append (Array.slice 0 (screenWidth - amount) row)
                else
                    row
            )
            screen
        )



-- |> Debug.log "rotateRow screen"


parseRect : String -> Instruction
parseRect instr =
    let
        dims =
            instr
                |> dropLeft 5
                |> String.split "x"

        w =
            dims
                |> head
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault -1

        h =
            dims
                |> tail
                |> Maybe.andThen head
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault -1
    in
        Rect ( Width w, Height h )


parseRow : String -> Instruction
parseRow instr =
    let
        row =
            instr
                |> dropLeft 11
                |> words
                |> head
                |> Maybe.withDefault ""
                |> dropLeft 2
                |> toInt
                |> Result.withDefault -1

        amount =
            instr
                |> words
                |> drop 4
                |> head
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault -1
    in
        RotateRow ( Row row, Amount amount )


parseCol : String -> Instruction
parseCol instr =
    let
        col =
            instr
                |> dropLeft 14
                |> words
                |> head
                |> Maybe.withDefault ""
                |> dropLeft 2
                |> toInt
                |> Result.withDefault -1

        amount =
            instr
                |> words
                |> drop 4
                |> head
                |> Maybe.withDefault ""
                |> toInt
                |> Result.withDefault -1
    in
        RotateCol ( Column col, Amount amount )


parseInstruction : String -> Instruction
parseInstruction instr =
    if String.contains "rect" instr then
        parseRect instr
    else if String.contains "row" instr then
        parseRow instr
    else if String.contains "column" instr then
        parseCol instr
    else
        Debug.crash "hmm?"


instructions : List Instruction
instructions =
    puzzleInput
        |> lines
        |> List.map parseInstruction



--VIEWS


screenView : Screen -> Html msg
screenView (Screen screen) =
    screen
        |> Array.toList
        |> List.map rowView
        |> (::) headerView
        |> table []


rowView : Array.Array Bool -> Html msg
rowView row =
    row
        |> Array.toList
        |> List.map colView
        |> (::) (td [] [ text "|" ])
        |> tr []


colView : Bool -> Html msg
colView col =
    if col then
        td [] [ text "#" ]
    else
        td [] [ text "." ]


headerView : Html msg
headerView =
    List.range 0 (screenWidth - 1)
        |> List.map (\v -> th [] [ v |> toString |> text ])
        |> (::) (th [] [ text "*" ])
        |> tr []
