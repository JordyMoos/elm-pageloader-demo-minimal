module Main exposing (main)

import Page.Error as Error
import Page.Home as Home
import Page.Numbers.LoadingNumbers as LoadingNumbers
import Page.Numbers.Numbers as Numbers
import PageLoader exposing (PageState(Loaded, Transitioning), TransitionStatus(..))
import PageLoader.Progression as Progression
import Html exposing (Html, div, button, hr, text)
import Html.Events exposing (onClick)


{-| You define pages like normally in a SPA
-}
type Page
    = ErrorPage Error.Model
    | HomePage
    | NumbersPage Numbers.Model


{-| And for pages that have dependencies that needs to be resolved, you also define loaders
-}
type Loading
    = LoadingNumbers LoadingNumbers.Model Progression.Progression


{-| Those pages and loader are given to the page state

The idea of the PageState is that it can either be:

  - `Loaded page` when a page is ready
  - `Transitioning page loader` when loading a new page. Where the `page` is actually the previous page and `loader` is the loader for the new page.

@see <http://package.elm-lang.org/packages/JordyMoos/elm-pageloader/2.0.1/PageLoader#PageState> for more information about `PageState`

-}
type alias Model =
    { pageState : PageState Page Loading
    }


{-| In this demo we always start with a the loaded HomePage.
-}
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


{-| Navigation is handled via Msg's. But can also be handled by the navigator.

@see <https://github.com/JordyMoos/elm-pageloader-demo-site> for a more advanced demo that uses the navigator

-}
type Msg
    = NoOp
    | ShowHome
    | ShowNumbers
    | LoadingNumbersMsg LoadingNumbers.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageState ) of
        {- If we get the ShowHome Msg then we set the pageState to `Loaded HomePage`
           This way you can transition directly to a page
        -}
        ( ShowHome, _ ) ->
            { model | pageState = Loaded HomePage } ! []

        {- If we get a ShowNumbers page then we want to load our random number page.
           But that page first needs some dependencies (it needs two random numbers)
           Therefor we set our pageState to `Transitioning oldPage LoadingNumbers`.

           We actually do that by calling the `LoadingNumbers.init` which returns a `TransitionStatus`.
           The TransitionStatus holds data about the transition/loader. Like data that is already figures out. Commands that needs to be fired.
           But also if the transition is pending (waiting for more data), failed (when something crucial went wrong) or success (with the resolved data).

           We can transform a transition status to a new page state. depending if it was failed/success.
           We can use helper methods like `processLoadingNumbers` for that (discussed below)

           @see http://package.elm-lang.org/packages/JordyMoos/elm-pageloader/2.0.1/PageLoader#TransitionStatus for more information about `TransitionStatus`
        -}
        ( ShowNumbers, _ ) ->
            let
                oldPage =
                    PageLoader.visualPage model.pageState
            in
                processLoadingNumbers oldPage LoadingNumbers.init
                    |> updatePageState model

        {- Msgs can be send to a loading page. Which in turn will again return a `TransitionStatus`.
           The `TransitionStatus` can be handled the same as when we initialized the loading
        -}
        ( LoadingNumbersMsg subMsg, Transitioning oldPage (LoadingNumbers subModel _) ) ->
            processLoadingNumbers oldPage (LoadingNumbers.update subMsg subModel)
                |> updatePageState model

        ( _, _ ) ->
            let
                _ =
                    Debug.log "Ignoring wrong message for state" (toString ( msg, model.pageState ))
            in
                ( model, Cmd.none )


{-| A `TransitionStatus` that is returned from the Loaders init or update function. Needs to be converted to a new PageState.

  - If the TransitionStatus is Failed then you want to show an ErrorPage
  - If the TransitionStatus is Success then you want the new page to be shown. With that you also need to specify the init function of the new page. And also a tagger to wrap Msgs from the new page.
  - If the transitionStatus is Pending then you need specify a tagger for the loaders Msgs

All with all there is a lot of configuration to convert a TransitionStatus to a PageState.
But the good thing is this is, that the configuration is the same for the init of a loading as for an update.
Therefor we only have to do this once per loader.'

We can use `PageLoader.defaultProcessLoading` to do all the state checking for us.
And all we have to do is specify the errorPage loader loaderMsgTagger destinationPage destinationInit and destinationMsgTagger

@see <http://package.elm-lang.org/packages/JordyMoos/elm-pageloader/2.0.1/PageLoader#defaultProcessLoading> for more information about `defaultProcessLoading`

Note: all functions starting with `default` are functions that are common to but can not be really what you want.
You can off course write your own.

-}
processLoadingNumbers : Page -> TransitionStatus LoadingNumbers.Model LoadingNumbers.Msg Numbers.Model -> ( PageState Page Loading, Cmd Msg )
processLoadingNumbers =
    PageLoader.defaultProcessLoading ErrorPage LoadingNumbers LoadingNumbersMsg NumbersPage Numbers.init (\_ -> NoOp)


updatePageState : Model -> ( PageState Page Loading, Cmd msg ) -> ( Model, Cmd msg )
updatePageState model ( pageState, cmd ) =
    ( { model | pageState = pageState }, cmd )


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
