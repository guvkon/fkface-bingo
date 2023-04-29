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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url, Cmd.none )


choices : List String
choices =
    [ "Night time shades"
    , "Double fisting"
    , "Separated at crosswalk"
    , "Dumb hat"
    , "Looser board"
    , "Skate board"
    , "Fanny pack"
    , "Matching outfit"
    , "Guy on guy scooter"
    , "Dead parrot head"
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
    , "Litter bug"
    , "Jeans in flip-flops"
    , "Mullet"
    , "Bachelorette party"
    , "Go-kart"
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
    , "Aggressive honking"
    , "Trip"
    , "Someone yells for someone to wait up"
    , "Nose pick"
    , "Beach goth"
    , "Yawning"
    , "Trying to get into a wrong car"
    , "Formal attire"
    , "Hoofing it"
    , "Offensive T-shirt"
    , "Handshake"
    ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GenerateBoard
    | NewBoard ( List String, List String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        GenerateBoard ->
            ( model, Random.generate NewBoard (Random.List.choices 24 choices) )

        NewBoard ( selected, _ ) ->
            case encodeBoard (makeBoard selected) of
                Just encodedBoard ->
                    ( model, Nav.pushUrl model.key ("/board/" ++ encodedBoard) )

                Nothing ->
                    ( model, Cmd.none )


makeBoard : List String -> Board
makeBoard values =
    List.map2 Tuple.pair (List.concat [ List.take 12 values, [ "Free Space" ], List.drop 12 values ]) (List.repeat 25 False)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case toRoute model.url of
        Home ->
            viewHome Nothing

        NotFound ->
            viewHome Nothing

        Board str ->
            viewHome (decodeBoard str)


viewHome : Maybe Board -> Browser.Document Msg
viewHome board =
    { title = "F**kFace Sloppy Joes Bingo"
    , body =
        [ div [ class "container my-4" ]
            [ h1 [ class "mb-3" ] [ text "F**kFace Sloppy Joes Bingo" ]
            , ul []
                [ li []
                    [ span [] [ text "F**kFace Sloppy Joes Bingo being played -> " ]
                    , a [ href "https://www.youtube.com/watch?v=kxsJ4PW_R04", target "_blank" ] [ text "[video]" ]
                    ]
                , li []
                    [ span [] [ text "Sloppy Joe's Key West Street cam -> " ]
                    , a [ href "https://liveduvalstreet.com/", target "_blank" ] [ text "[stream]" ]
                    ]
                ]
            , viewBoard board
            ]
        ]
    }


viewBoard : Maybe Board -> Html Msg
viewBoard maybeBoard =
    case maybeBoard of
        Nothing ->
            button [ class "btn btn-primary my-3", onClick GenerateBoard ] [ text "Generate Board" ]

        Just board ->
            div [ class "mt-3" ]
                [ p []
                    [ text "Board link: "
                    , viewBoardLink board
                    ]
                , button [ class "btn btn-danger", onClick GenerateBoard ] [ text "Re-Generate Board" ]
                , viewJustBoard board
                ]


viewJustBoard : Board -> Html msg
viewJustBoard board =
    div [ class "mt-4" ]
        (List.indexedMap viewCell board)


viewCell : Int -> BoardCell -> Html msg
viewCell index ( cell, click ) =
    div [] [ text (String.fromInt index ++ ". " ++ cell) ]


viewBoardLink : Board -> Html msg
viewBoardLink board =
    let
        maybeEncodedBoard =
            encodeBoard board
    in
    case maybeEncodedBoard of
        Just encodedBoard ->
            a [ href ("/board/" ++ encodedBoard), target "_blank" ] [ text (String.slice 0 32 encodedBoard) ]

        Nothing ->
            text ""


encodeBoard : Board -> Maybe String
encodeBoard board =
    let
        values =
            List.map (\( cell, _ ) -> cell) board
    in
    Bytes.Encode.string (List.foldl (\a b -> a ++ "," ++ b) "" values)
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
