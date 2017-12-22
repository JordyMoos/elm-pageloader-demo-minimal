module Page.Numbers.LoadingNumbers exposing (Model, Msg(..), init, update)

import Page.Numbers.Numbers as Numbers
import PageLoader exposing (TransitionStatus)
import PageLoader.Progression as Progression
import PageLoader.DependencyStatus as DependencyStatus
import Process
import Random
import Task


{- The loaders job is to get all the data before transitioning to the new page -}


{-| Here we specify that we need two random numbers.
This can be more efficient but this is for a demo purposes
-}
type alias Model =
    { first : Maybe Int
    , second : Maybe Int
    }


type Msg
    = RandomFirst Int
    | RandomSecond Int


{-| When we initialize the loader we must answer with a `TransitionStatus`
-}
init : TransitionStatus Model Msg Numbers.Model
init =
    asTransitionStatus <|
        { first = Nothing -- Say we dont have anything yet
        , second = Nothing
        }
            ! [ randomNumberCommand RandomFirst -- Commands to fetch the data in some way
              , randomNumberCommand RandomSecond
              ]


{-| We must also answer with a `TransitionStatus` when we get an update.
-}
update : Msg -> Model -> TransitionStatus Model Msg Numbers.Model
update msg model =
    asTransitionStatus <|
        case msg of
            RandomFirst number ->
                { model | first = Just number } ! []

            RandomSecond number ->
                { model | second = Just number } ! []


{-| Whenever our model is changed, we can convert out tuple of (Model, Cmd Msg) together with a list of `DependencyStatus.Status` to a new `TransitionStatus`
We also specify a closure which is called when the TransitionStatus is `Success.
This closure is used to create the resulting data. Which is given to the new page init function.
I often use the new page Model. But this is not required.
-}
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


{-| All our dependencies needs to be converted to something of the same type.
So we can use them in a list and test what the state of the dependencies are.

That is done by converting your dependencies to a `DependencyStatus.Status`

-}
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
