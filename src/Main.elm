module Main exposing (main)

import Page.Error as Error
import Page.Home as Home
import Page.Numbers.LoadingNumbers as LoadingNumbers
import Page.Numbers.Numbers as Numbers
import PageLoader exposing (PageState(Loaded, Transitioning), TransitionStatus(..))
import PageLoader.Progression as Progression
import Html exposing (Html, div, button, hr, text)
import Html.Events exposing (onClick)


type Page
    = ErrorPage Error.Model
    | HomePage
    | NumbersPage Numbers.Model


type Loading
    = LoadingNumbers LoadingNumbers.Model Progression.Progression


type alias Model =
    { pageState : PageState Page Loading
    }


initModel : Model
initModel =
    { pageState = Loaded HomePage }


main : Program Never Model Msg
main =
    Html.program
        { init = initModel ! []
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type Msg
    = NoOp
    | ShowHome
    | ShowNumbers
    | LoadingNumbersMsg LoadingNumbers.Msg


processLoadingNumbers : Page -> TransitionStatus LoadingNumbers.Model LoadingNumbers.Msg Numbers.Model -> ( PageState Page Loading, Cmd Msg )
processLoadingNumbers =
    PageLoader.defaultProcessLoading ErrorPage LoadingNumbers LoadingNumbersMsg NumbersPage Numbers.init (\_ -> NoOp)


updatePageState : Model -> ( PageState Page Loading, Cmd msg ) -> ( Model, Cmd msg )
updatePageState model ( pageState, cmd ) =
    ( { model | pageState = pageState }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageState ) of
        ( LoadingNumbersMsg subMsg, Transitioning oldPage (LoadingNumbers subModel _) ) ->
            processLoadingNumbers oldPage (LoadingNumbers.update subMsg subModel)
                |> updatePageState model

        ( ShowHome, _ ) ->
            { model | pageState = Loaded HomePage } ! []

        ( ShowNumbers, _ ) ->
            let
                oldPage =
                    PageLoader.visualPage model.pageState
            in
                processLoadingNumbers oldPage LoadingNumbers.init
                    |> updatePageState model

        ( _, _ ) ->
            let
                _ =
                    Debug.log "Ignoring wrong message for state" (toString ( msg, model.pageState ))
            in
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage page

        Transitioning oldPage loader ->
            viewPage oldPage


viewPage : Page -> Html Msg
viewPage page =
    wrapContent <|
        case page of
            ErrorPage model ->
                Error.view model

            HomePage ->
                Home.view

            NumbersPage model ->
                Numbers.view model


wrapContent : Html Msg -> Html Msg
wrapContent content =
    div
        []
        [ content
        , hr [] []
        , div
            []
            [ button [ onClick ShowHome ] [ text "Home" ] ]
        , div
            []
            [ button [ onClick ShowNumbers ] [ text "Random Numbers" ] ]
        ]
