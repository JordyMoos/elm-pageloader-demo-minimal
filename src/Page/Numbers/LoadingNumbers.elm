module Page.Numbers.LoadingNumbers exposing (Model, Msg(..), init, update)

import Page.Numbers.Numbers as Numbers
import PageLoader exposing (TransitionStatus)
import PageLoader.Progression as Progression
import PageLoader.DependencyStatus as DependencyStatus
import Process
import Random
import Task


type alias Model =
    { first : Maybe Int
    , second : Maybe Int
    }


type Msg
    = RandomFirst Int
    | RandomSecond Int


init : TransitionStatus Model Msg Numbers.Model
init =
    asTransitionStatus <|
        { first = Nothing
        , second = Nothing
        }
            ! [ randomNumberCommand RandomFirst
              , randomNumberCommand RandomSecond
              ]


update : Msg -> Model -> TransitionStatus Model Msg Numbers.Model
update msg model =
    asTransitionStatus <|
        case msg of
            RandomFirst number ->
                { model | first = Just number } ! []

            RandomSecond number ->
                { model | second = Just number } ! []


asTransitionStatus : ( Model, Cmd Msg ) -> TransitionStatus Model Msg Numbers.Model
asTransitionStatus ( model, cmd ) =
    PageLoader.defaultDependencyStatusListHandler
        ( model, cmd )
        (dependencyStatuses model)
        (\() ->
            { first = Maybe.withDefault 0 model.first
            , second = Maybe.withDefault 0 model.second
            }
        )


dependencyStatuses : Model -> List DependencyStatus.Status
dependencyStatuses model =
    [ maybeAsStatus model.first
    , maybeAsStatus model.second
    ]


randomNumberCommand : (Int -> Msg) -> Cmd Msg
randomNumberCommand tagger =
    Random.int 1 6
        |> Random.generate tagger


maybeAsStatus : Maybe a -> DependencyStatus.Status
maybeAsStatus maybe =
    case maybe of
        Just _ ->
            DependencyStatus.Success

        Nothing ->
            DependencyStatus.Pending Progression.singlePending
