module Main exposing (main)

import Browser
import Game
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text "gello"
        ]
