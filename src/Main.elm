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
    [ "Night time shades"
    , "Double fisting"
    , "Separated at the crosswalk"
    , "Dumb hat"
    , "Loser board"
    , "Skate board"
    , "Fanny pack"
    , "Matching outfit"
    , "Guy on guy scooter"
    , "Dead Parrot Head"
    , "Crosswalk coward"
    , "Shirtless dude"
    , "Memaw"
    , "Kid out too late"
    , "Ghost tour bus / Street train"
    , "Trycicle"
    , "Pink taxi"
    , "Bouncer bounces"
    , "Drop something"
    , "Drop/spill food/drink"
    , "Litterbug"
    , "Jeans in flip-flops"
    , "Mullet"
    , "Bachelorette party"
    , "Go cart"
    , "Luxury car"
    , "Drunk walk"
    , "Wheel chair"
    , "Camera shy"
    , "Camera acknowledge"
    , "Bandage dude"
    , "Pickup / Dropoff"
    , "Dancing"
    , "Twerking"
    , "Sitting on the street"
    , "Chug"
    , "Hug"
    , "Aggressive honking"
    , "Trip"
    , "Someone yells for someone to wait up"
    , "Nose pick"
    , "Beach goth"
    , "Yawning"
    , "Trying to get into a wrong car"
    , "Formal attire"
    , "Hoofin it"
    , "Offensive T-shirt"
    , "Handshake"
    , "Couple fighting / regular fight"
    , "Cop car / ambulance / fire truck"
    , "Crying"
    , "Sucking on a chili dog"
    , "High five"
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
    List.map2 Tuple.pair (List.concat [ List.take 12 values, [ "Free Space" ], List.drop 12 values ]) (List.repeat 25 False)


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
viewHome board =
    { title = "F**kFace Sloppy Joe's Bingo"
    , body =
        [ div [ class "container mt-5" ]
            [ h1 [ class "mb-3" ] [ text "F**kFace Sloppy Joe's Bingo" ]
            , ul []
                [ li []
                    [ span [] [ text "F**kFace Sloppy Joe's Bingo being played -> " ]
                    , a [ href "https://www.youtube.com/watch?v=kxsJ4PW_R04", target "_blank" ] [ text "[video]" ]
                    ]
                , li []
                    [ span [] [ text "Sloppy Joe's Key West Street cam -> " ]
                    , a [ href "https://liveduvalstreet.com/", target "_blank" ] [ text "[stream]" ]
                    ]
                ]
            , generateButton board
            ]
        , viewBoard board
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


viewBoard : Maybe Board -> Html Msg
viewBoard maybeBoard =
    case maybeBoard of
        Nothing ->
            div [] []

        Just board ->
            div [ class "container mt-4 mb-5" ]
                [ div [ class "board" ] (List.indexedMap viewCell board)
                ]


viewCell : Int -> BoardCell -> Html Msg
viewCell index ( cell, state ) =
    if cell == "Free Space" then
        button [ class "cell freespace", disabled True ] []

    else if state then
        button [ class "cell clicked", onClick (CellClicked index) ]
            [ div [] [ text cell ]
            , img [ src "/fkface.webp", class "overlay" ] []
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
            List.map2 Tuple.pair values (List.repeat 25 False)
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
