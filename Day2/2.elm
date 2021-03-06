module Main exposing (..)

import Html exposing (..)
import String exposing (..)


main : Html a
main =
    text (toString (List.map coordsToNumber <| walk model coordsArr))


type alias Model =
    { position : ( Int, Int )
    , code : String
    }


model : Model
model =
    Model ( -2, 0 ) ""


move : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
move ( targetX, targetY ) ( currentX, currentY ) =
    --Name target is misleading. It's direction in which to go
    let
        current =
            Debug.log "current"
                ( currentX, currentY )

        target =
            Debug.log "target"
                ( targetX, targetY )

        ( resultX, resultY ) =
            Debug.log "result"
                ( currentX + targetX, currentY + targetY )

        newLine =
            Debug.log "" ""
    in
        if Debug.log "result?" (abs resultX + abs resultY) > 2 then
            ( currentX, currentY )
        else
            ( resultX, resultY )


walk : Model -> List (List ( Int, Int )) -> List ( Int, Int )
walk model instructions =
    List.map (List.foldl move model.position) (Debug.log "instructions" instructions)


strMoveToCoords : String -> ( Int, Int )
strMoveToCoords move =
    case move of
        "R" ->
            ( 1, 0 )

        "L" ->
            ( -1, 0 )

        "U" ->
            ( 0, 1 )

        "D" ->
            ( 0, -1 )

        _ ->
            Debug.crash "shit happens"


targetCoordsV2 : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
targetCoordsV2 ( currentX, currentY ) ( targetX, targetY ) =
    let
        targetX =
            100
    in
        ( targetX, targetY )


coordsToNumber : ( Int, Int ) -> String
coordsToNumber coords =
    case coords of
        ( 0, 2 ) ->
            "1"

        ( -1, 1 ) ->
            "2"

        ( 0, 1 ) ->
            "3"

        ( 1, 1 ) ->
            "4"

        ( -2, 0 ) ->
            "5"

        ( -1, 0 ) ->
            "6"

        ( 0, 0 ) ->
            "7"

        ( 1, 0 ) ->
            "8"

        ( 2, 0 ) ->
            "9"

        ( -1, -1 ) ->
            "A"

        ( 0, -1 ) ->
            "B"

        ( 1, -1 ) ->
            "C"

        ( 0, -2 ) ->
            "D"

        _ ->
            Debug.crash "error"


coordsArr : List (List ( Int, Int ))
coordsArr =
    List.map (List.map strMoveToCoords) inputArr


inputArr : List (List String)
inputArr =
    List.map (String.split "") <| lines input


input : String
input =
    """RLRDDRLLDLRLUDDULLDRUUULDDLRLUDDDLDRRDUDDDLLURDDDLDDDRDURUDRDRRULUUDUDDRRRLRRRRRLRULRLLRULDRUUDRLRRURDDRLRULDLDULLLRULURRUULLRLLDDDDLLDURRUDLDLURDRDRDLUUUDDRDUUDDULLUURRDRLDDULURRRUDLLULULDLLURURUDRRRRUDRLRDLRRLDDRDDLULDLLLURURDUDRRRRUULURLRDULDRLUDRRUDDUULDURUDLDDURRRDLULLUUDRLLDUUDLDRUDDRLLLLLLDUDUDDLRDLRRDRUDDRRRLLRRDLLRLDDURUURRRDDLDUULLDLDLRURDLLLDDRUUDRUDDDDULRLLDUULRUULLLULURRRLLULDLDUDLDLURUDUDULLDLLUUDRRDRLUURURURURDLURUUDLDRLUDDUUDULDULULLLDLDDULLULLDULRRDRULLURRRULLDDDULULURLRDURLLURUDDULLRUDLRURURRDRDUULDRUUDURDURDDLRDUUULDUUDRDURURDRRRURLLDDLLLURURULULUDLRDLDRDRURLRLULRDLU\x0D
UDLDURRULDRDDLDUULUDLDUULUURDDRUDRURRRUDRURLLDDRURLDLRDUUURDLLULURDDUDDDRRRURLLDLDLULRDULRLULDLUUDLLRLDLRUUULDDUURDLDDRRDLURLDUDDRURDRRURDURRRLUULURDDLRDLDRRRLDUDRLRLLRLDDUULDURUUULLLRRRRRRRDRRRDRLUULDLDDLULDRDUDLLUDRRUDRUUDULRLUURDDDDRRUUDLURULLLURDULUURDRDDURULRUDRRDLRDUUUUUDDDRDRDDRUDRDDDRLRUUDRDRDDDLUDRDRLDRDDRULURDRLDRUDUDRUULRLLUDRDRLLLLDUDRRLLURDLLLDRRUDDUDRLRLDUDRLURRUUULURDDRUURRLDRLRRRUUDLULDDDRDLDUUURLLUULDDRRUDLDDRUDUDUURURDDRDULLLLLULRRRDLRRRDDDLURDDDDLUULLLRDDURRRRLURRLDDLRUULULRDRDDDDLDUUUUUUDRRULUUUDD\x0D
UURDRRUDLURRDDDLUDLRDURUDURDLLLLRDLRLRDDRDRDUUULRDLLDLULULRDUDDRRUUDURULDLUDLRDRUDLDDULLLDDRDLLDULLLURLLRDDLDRDULRRDDULRDURLLRUDRLRRLUDURLDRDLDLRLLLURLRRURDLDURDLUDULRDULLLDRDDRDLDRDULUULURDRRRLDRRUULULLDDRRLDLRUURLRUURLURRLLULUUULRLLDDUDDLRLDUURURUDLRDLURRLLURUDLDLLUDDUULUUUDDDURDLRRDDDLDRUDRLRURUUDULDDLUUDDULLDDRRDDRRRUDUDUDLDLURLDRDLLLLDURDURLRLLLUUDLRRRRUDUDDLDLRUURRLRRLUURRLUDUDRRRRRRRLDUDDRUDDLUDLRDDDRLDUULDRDRRDLDRURDLDRULRLRLUDRDLRRUURUUUUDLDUUULLLRRRRRDLRRURDDLLLLUULDLLRULLUDLLDLLUDLRLRRLRURDDRRL\x0D
URDRDLLRDDDLLLDDLURLRURUURRRLUURURDURRLLUDURRLRLDLUURDLULRRDRUDDLULDLDRLDLRLRRLLLDDDUDDDLRURURRLLDRRRURUDLRDDLLDULDDLDRLUUUDRRRULDUULRDDDLRRLLURDDURLULRDUDURRLLDLLRLDUDDRRDDLRLLLDUDRLUURRLLDULRLDLUUUUUDULUDLULUDDUURRURLDLDRRLDLRRUDUDRRDLDUDDLULLDLLRDRURDRDRRLDDDDRDDRLLDDDLLUDRURLURDRRRRRUDDDUDUDDRDUUDRRUDUDRLULDDURULUURUUUURDRULRLRULLDDRRRUULRRRRURUDLDLRDLLDRLURLRUULLURDUDULRRURLRLLRRLLLURULRRRLDDUULLUUULRRDRULUUUUDRDRRDLRURLRLLRLRRRDRDRLDLUURUURULLDLULRRLRRDRULRRLLLDDURULLDLDLDLUUURDLDLUUDULRLLUDDRRDLLDLDLDURLUURRDDRRURDRLUDRLUUUDLDULDLUDRLDUDDLLRUDULLLLLDRRLLUULLUUURRDDUURDLLRDDLRLLU\x0D
LDUDRRDLUUDDRLLUUULURLDUDLUDLRLDRURLULRLLDDLRRUUUDDDDRDULDDUUDLRUULDRULLRDRUDDURLDUUURRUDUDRDRDURRDLURRRDRLDLRRRLLLRLURUURRDLLRDLDDLLRDUDDRDUULRULRRURLUDDUDDDUULLUURDULDULLLLRUUUDDRRRLDDDLDLRRDRDRDLUULRLULDRULDLRDRRUDULUDLLUDUULRDLRRUUDDLLDUDDRULURRLULDLDRRULDDRUUDDLURDLRDRLULRRLURRULDUURDLUDLLDRLDULLULDLLRDRDLLLUDLRULLRLDRDDDLDDDLRULDLULLRUUURRLLDUURRLRLDUUULDUURDURRULULRUUURULLLRULLURDDLDRLLRDULLUDLDRRRLLLLDUULRRLDURDURDULULDUURLDUDRLRURRDLUUULURRUDRUUUDRUR"""
