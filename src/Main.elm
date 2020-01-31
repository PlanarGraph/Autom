module Main exposing (..)

import Autom exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { exp : String
    }


init : Model
init =
    Model ""


type Msg
    = Exp String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Exp s ->
            { model | exp = s }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Regex", value model.exp, onInput Exp ] []
        , validate model.exp
        ]


validate : String -> Html Msg
validate exp =
    if isValid simpleDFA simpleDFA.start exp then
        div [] [ text "Valid!" ]

    else
        div [] [ text "Invalid!" ]
