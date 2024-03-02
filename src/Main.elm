module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Game
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest req ->
            ( model
            , Cmd.none
            )

        OnUrlChange url ->
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
    Html.div []
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
        (List.map viewNumber numbers)


viewNumber : Game.Number -> Html Msg
viewNumber number =
    Html.div [ Attributes.class "number" ]
        [ Html.text (numberToString number)
        ]


numberToString : Game.Number -> String
numberToString number =
    String.fromInt (Game.numberToInt number)
