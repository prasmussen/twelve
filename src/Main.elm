module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Navigation
import Game
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Random
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { gameModel : Game.Model
    }


type alias Flags =
    { seed : Int
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags _ _ =
    ( { gameModel =
            --Random.initialSeed flags.seed
            --    |> Game.randomFields
            --    |> Game.init
            Game.init Game.knownFields
      }
    , Cmd.none
    )


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | ShiftLeftClicked
    | ShiftRightClicked
    | SwapClicked
    | KeyPressed Key


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.keyCode
        |> Decode.andThen keyDecoder
        |> Decode.map KeyPressed
        |> Browser.Events.onKeyDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest _ ->
            ( model
            , Cmd.none
            )

        OnUrlChange _ ->
            ( model
            , Cmd.none
            )

        ShiftLeftClicked ->
            ( { model | gameModel = Game.shiftLeft model.gameModel }
            , Cmd.none
            )

        ShiftRightClicked ->
            ( { model | gameModel = Game.shiftRight model.gameModel }
            , Cmd.none
            )

        SwapClicked ->
            ( { model | gameModel = Game.swap model.gameModel }
            , Cmd.none
            )

        KeyPressed key ->
            case key of
                KeyLeft ->
                    ( { model | gameModel = Game.shiftLeft model.gameModel }
                    , Cmd.none
                    )

                KeyUp ->
                    ( { model | gameModel = Game.swap model.gameModel }
                    , Cmd.none
                    )

                KeyRight ->
                    ( { model | gameModel = Game.shiftRight model.gameModel }
                    , Cmd.none
                    )

                KeyDown ->
                    ( { model | gameModel = Game.swap model.gameModel }
                    , Cmd.none
                    )


view : Model -> Browser.Document Msg
view model =
    { title = "Twelve Puzzle"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    let
        numbers =
            Game.fieldsToList (Game.fields model.gameModel)
    in
    Html.div [ Attributes.class "game" ]
        [ viewNumbers numbers
        , viewControls
        , viewStatus model
        ]


viewControls : Html Msg
viewControls =
    Html.div [ Attributes.class "controls" ]
        [ Html.button [ Events.onClick ShiftLeftClicked ] [ Html.text "Shift left" ]
        , Html.button [ Events.onClick SwapClicked ] [ Html.text "Swap" ]
        , Html.button [ Events.onClick ShiftRightClicked ] [ Html.text "Shift right" ]
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    let
        text =
            if Game.isSolved model.gameModel then
                "Solved? Yes!"

            else
                "Solved? Nope"
    in
    Html.div [ Attributes.class "status" ]
        [ Html.text text
        ]


viewNumbers : List Game.Number -> Html Msg
viewNumbers numbers =
    Html.div [ Attributes.class "numbers" ]
        (List.indexedMap viewNumber numbers)


viewNumber : Int -> Game.Number -> Html Msg
viewNumber index number =
    Html.div
        [ Attributes.class "number"
        , Attributes.class ("field-" ++ String.fromInt (index + 1))
        ]
        [ Html.text (numberToString number)
        ]



-- KEY


type Key
    = KeyLeft
    | KeyUp
    | KeyRight
    | KeyDown


keyDecoder : Int -> Decoder Key
keyDecoder code =
    case code of
        37 ->
            Decode.succeed KeyLeft

        38 ->
            Decode.succeed KeyUp

        39 ->
            Decode.succeed KeyRight

        40 ->
            Decode.succeed KeyDown

        _ ->
            Decode.fail "Unknown key"



-- HELPERS


numberToString : Game.Number -> String
numberToString number =
    String.fromInt (Game.numberToInt number)
