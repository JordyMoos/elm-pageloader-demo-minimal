module Page.Numbers.Numbers exposing (Model, init, view)

import Html exposing (..)


type alias Model =
    { first : Int
    , second : Int
    }


init : Model -> ( Model, Cmd msg )
init model =
    model ! []


view : Model -> Html msg
view model =
    div
        []
        [ h1 [] [ text "Random numbers page" ]
        , div []
            [ p [] [ text <| "The preloaded random numbers are: " ++ (toString model) ]
            ]
        ]
