module Page.Numbers.Numbers exposing (Model, init, view)

import Html exposing (..)


{- The Numbers page if a really plain page.
   It does not need to worry about getting the random numbers.
   Or testing for Maybe's. It knows that it has the numbers and therefor makes this page really clean
-}


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
