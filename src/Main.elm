module Main exposing (..)

import Base64
import Browser
import Browser.Navigation as Nav
import Bytes
import Bytes.Decode
import Bytes.Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.List
import Set
import Tuple
import Url
import Url.Parser exposing ((</>))


type Route
    = Home
    | Board String
    | NotFound


route : Url.Parser.Parser (Route -> a) a
route =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Board (Url.Parser.s "board" </> Url.Parser.string)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (Url.Parser.parse route url)


urlToBoard : Maybe Url.Url -> Maybe Board
urlToBoard maybeUrl =
    case maybeUrl of
        Just url ->
            case toRoute url of
                NotFound ->
                    Nothing

                Home ->
                    Nothing

                Board encodedString ->
                    decodeBoard encodedString

        Nothing ->
            Nothing



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Board =
    List BoardCell


type alias BoardCell =
    ( String, Bool )


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , board : Maybe Board
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url (urlToBoard (Just url)), Cmd.none )


choices : List String
choices =
    [ "Mullet"
    , "Ghost Tour Bus / Street Train"
    , "Dropped something"
    , "Couple fighting / regular fight"
    , "Car delivery / pickup"
    , "Tricycle"
    , "Puking on ground / trash can"
    , "Dropped / spilled food and drink"
    , "Beach goth"
    , "Cop car / ambulance / fire truck"
    , "Trip"
    , "Almost get hit by car"
    , "Bachelorette party"
    , "Bandage dude"
    , "Wheelchair"
    , "Acknowledge camera"
    , "Selfie"
    , "Dumb hat"
    , "Offensive t shirt"
    , "Kid out late"
    , "Nose pick / wedgie pull / crotch grab / spit"
    , "Crying"
    , "Bouncer bouncing"
    , "Dancing"
    , "Luxury car"
    , "Sitting on the street"
    , "Dead Parrot Head"
    , "Can hear the cover band playing Green Day or Blink 182"
    , "American flag clothes"
    , "Cowboy hat"
    , "Can see FaceTime screen"
    , "Aggressive honking"
    , "High five"
    , "Handshake"
    , "Hug"
    , "Litterbug"
    , "Drunk walk"
    , "Jeans and flip flops"
    , "Guy-on-guy scooter"
    , "Meemaw"
    , "Hoofin it"
    , "Barefoot"
    , "Camera shy"
    , "Pink taxi"
    , "Doublefisting"
    , "Formal attire"
    , "Go cart"
    , "Skateboard"
    , "Someone yells to wait up"
    , "Twerking"
    , "Nighttime shades"
    , "Crosswalk coward"
    , "Separated at the crosswalk"
    , "Sucking on a chili dog"
    , "Yawning"
    , "Matching outfits"
    , "Trying to get into the wrong car"
    , "Chug"
    ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GenerateBoard
    | NewBoard ( List String, List String )
    | CellClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | board = urlToBoard (Just url) }, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, board = urlToBoard (Just url) }
            , Cmd.none
            )

        GenerateBoard ->
            ( model, Random.generate NewBoard (Random.List.choices 24 choices) )

        NewBoard ( selected, _ ) ->
            case encodeBoard (makeBoard selected) of
                Just encodedBoard ->
                    ( { model | board = urlToBoard (Url.fromString ("/board/" ++ encodedBoard)) }, Nav.pushUrl model.key ("/board/" ++ encodedBoard) )

                Nothing ->
                    ( model, Cmd.none )

        CellClicked position ->
            ( { model
                | board =
                    case model.board of
                        Nothing ->
                            Nothing

                        Just board ->
                            Just (clickCell board position)
              }
            , Cmd.none
            )


makeBoard : List String -> Board
makeBoard values =
    List.map2 Tuple.pair (List.concat [ List.take 12 values, [ "Free Space" ], List.drop 12 values ]) defaultSelects


defaultSelects : List Bool
defaultSelects =
    let
        map idx _ =
            if idx == 12 then
                True

            else
                False
    in
    List.repeat 25 False
        |> List.indexedMap map


clickCell : Board -> Int -> Board
clickCell board clickedOn =
    let
        toggleCell : Int -> BoardCell -> BoardCell
        toggleCell position ( value, state ) =
            if position == clickedOn then
                ( value, not state )

            else
                ( value, state )
    in
    List.indexedMap toggleCell board



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    viewHome model.board


viewHome : Maybe Board -> Browser.Document Msg
viewHome maybeBoard =
    let
        title =
            "Sloppy Joe's Bingo Card Generator"
    in
    { title = title
    , body =
        [ div [ class "container-fluid container-lg mt-3" ]
            [ img [ src "/header.jpg", class "d-block w-100 header mb-3" ] []
            , h1 [ class "mb-3 d-none" ] [ text title ]
            , div [ class "d-none" ]
                [ img [ src "/fkface.webp" ] []
                , img [ src "/fkface-red.png" ] []
                ]
            , ul []
                [ li []
                    [ span [] [ text "Sloppy Bingo being played -> " ]
                    , a [ href "https://www.youtube.com/watch?v=kxsJ4PW_R04", target "_blank" ] [ text "[video]" ]
                    ]
                , li []
                    [ span [] [ text "Sloppy Joe's Key West Street cam -> " ]
                    , a [ href "https://liveduvalstreet.com/", target "_blank" ] [ text "[stream]" ]
                    ]
                ]
            , generateButton maybeBoard
            ]
        , case maybeBoard of
            Nothing ->
                div [] []

            Just board ->
                div []
                    [ viewResults board
                    , viewBoard board
                    ]
        , footer [ class "container-fluid container-lg py-4" ] []
        ]
    }


generateButton : Maybe Board -> Html Msg
generateButton maybeBoard =
    case maybeBoard of
        Nothing ->
            button [ class "btn btn-primary my-3", onClick GenerateBoard ] [ text "Generate Board" ]

        Just board ->
            div []
                [ p []
                    [ text "Board link: "
                    , viewBoardLink board
                    ]
                , button [ class "btn btn-danger btn-sm", onClick GenerateBoard ] [ text "Re-Generate Board" ]
                ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [ class "container-fluid container-lg my-4" ]
        [ div [ class "board" ] (List.indexedMap (\idx cell -> viewCell idx cell (isWinningCell board idx)) board)
        ]


viewCell : Int -> BoardCell -> Bool -> Html Msg
viewCell index ( cell, state ) hasWon =
    let
        addCls =
            if hasWon then
                " won"

            else
                ""
    in
    if cell == "Free Space" then
        button [ class ("cell freespace" ++ addCls), disabled True ] []

    else if state then
        button [ class ("cell clicked" ++ addCls), onClick (CellClicked index) ]
            [ div [] [ text cell ]
            , img
                [ if hasWon then
                    src "/fkface-red.png"

                  else
                    src "/fkface.webp"
                , class "overlay"
                ]
                []
            ]

    else
        button [ class "cell", onClick (CellClicked index) ] [ text cell ]


viewBoardLink : Board -> Html msg
viewBoardLink board =
    let
        maybeEncodedBoard =
            encodeBoard board
    in
    case maybeEncodedBoard of
        Just encodedBoard ->
            a [ href ("/board/" ++ encodedBoard) ] [ text (String.slice 0 32 encodedBoard) ]

        Nothing ->
            text ""


viewResults : Board -> Html msg
viewResults board =
    div [ class "container-fluid container-lg my-4 results" ]
        [ p [ class "mb-0" ]
            [ text
                (if fullBingo board then
                    "Full bingo has been achieved!"

                 else if bingo board then
                    "Congratulations! You're a bingo getter now. But surely you can get one more:"

                 else
                    "Bingo list:"
                )
            ]
        , ul []
            [ li [ class (bingoAchievedClass (List.member freeSpace (bingoWonPositions board))) ] [ text "Bingo with F**kFace space" ]
            , li [ class (bingoAchievedClass (isWinningAnyLine board horizontalLines)) ] [ text "Horizontal bingo" ]
            , li [ class (bingoAchievedClass (isWinningAnyLine board verticalLines)) ] [ text "Vertical bingo" ]
            , li [ class (bingoAchievedClass (isWinningAnyLine board diagonalLines)) ] [ text "Diagonal bingo" ]
            , li [ class (bingoAchievedClass (isWinningAnyLine board offCenterLines)) ] [ text "Bingo without F**kFace space" ]
            , li [ class (bingoAchievedClass (fullBingo board)) ] [ text "Full bingo" ]
            ]
        ]


bingoAchievedClass : Bool -> String
bingoAchievedClass achieved =
    if achieved then
        "achieved"

    else
        ""


encodeBoard : Board -> Maybe String
encodeBoard board =
    let
        values =
            List.map (\( cell, _ ) -> cell) board
    in
    Bytes.Encode.string (List.foldr (\a b -> a ++ "," ++ b) "" values)
        |> Bytes.Encode.encode
        |> Base64.fromBytes


decodeBoard : String -> Maybe Board
decodeBoard encoded =
    let
        valuesToBoard values =
            List.map2 Tuple.pair values defaultSelects
    in
    case Base64.toBytes encoded of
        Just bytes ->
            case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes of
                Just string ->
                    Just (valuesToBoard (String.split "," string))

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


bingoWonPositions : Board -> List Int
bingoWonPositions board =
    let
        lines =
            List.concat
                [ horizontalLines
                , verticalLines
                , diagonalLines
                ]
    in
    List.filter (isWinningLine board) lines
        |> List.concat
        |> Set.fromList
        |> Set.toList


diagonalLines : List (List Int)
diagonalLines =
    [ [ 0, 6, 12, 18, 24 ]
    , [ 4, 8, 12, 16, 20 ]
    ]


horizontalLines : List (List Int)
horizontalLines =
    [ List.range 0 4
    , List.range 5 9
    , List.range 10 14
    , List.range 15 19
    , List.range 20 24
    ]


verticalLines : List (List Int)
verticalLines =
    [ [ 0, 5, 10, 15, 20 ]
    , [ 1, 6, 11, 16, 21 ]
    , [ 2, 7, 12, 17, 22 ]
    , [ 3, 8, 13, 18, 23 ]
    , [ 4, 9, 14, 19, 24 ]
    ]


offCenterLines : List (List Int)
offCenterLines =
    [ List.range 0 4
    , List.range 5 9
    , List.range 15 19
    , List.range 20 24
    , [ 0, 5, 10, 15, 20 ]
    , [ 1, 6, 11, 16, 21 ]
    , [ 3, 8, 13, 18, 23 ]
    , [ 4, 9, 14, 19, 24 ]
    ]


freeSpace : Int
freeSpace =
    12


isWinningLine : Board -> List Int -> Bool
isWinningLine board line =
    List.foldl (\idx acc -> acc && List.member idx (positionsClicked board)) True line


isWinningAnyLine : Board -> List (List Int) -> Bool
isWinningAnyLine board lines =
    List.foldl (\line acc -> acc || isWinningLine board line) False lines


isWinningCell : Board -> Int -> Bool
isWinningCell board pos =
    bingoWonPositions board
        |> List.member pos


bingo : Board -> Bool
bingo board =
    bingoWonPositions board
        |> List.isEmpty
        |> not


fullBingo : Board -> Bool
fullBingo board =
    bingoWonPositions board
        |> List.length
        |> (==) 25


positionsClicked : Board -> List Int
positionsClicked board =
    List.indexedMap
        (\idx ( _, clicked ) ->
            if clicked then
                idx

            else
                -1
        )
        board
        |> List.filterMap
            (\idx ->
                if idx == -1 then
                    Nothing

                else
                    Just idx
            )
