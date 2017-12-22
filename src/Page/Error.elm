module Page.Error exposing (Model, view)

import Html exposing (..)


type alias Model =
    String


view : Model -> Html msg
view error =
    div
        []
        [ h1 [] [ text "Ooops error" ]
        , p [] [ text error ]
        ]
