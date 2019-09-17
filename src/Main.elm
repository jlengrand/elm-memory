module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Element exposing (Element, el)
import Element.Background
import Element.Font
import Element.Input
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import List.Extra exposing (groupsOf)
import Random
import Random.List
import Task
import Time exposing (Posix)



---- MODEL ----


numberOfRows : Int
numberOfRows =
    4


numberOfColumns : Int
numberOfColumns =
    4


numberOfPokemonsNeeded : Int
numberOfPokemonsNeeded =
    (numberOfColumns * numberOfRows) // 2


numberOfPokemons : Int
numberOfPokemons =
    13


redPokemon =
    Element.rgb255 197 32 24


yellowPokemon =
    Element.rgb255 255 246 164


whitePokemon =
    Element.rgb255 255 255 255


brownPokemon =
    Element.rgb255 98 49 8


type CardState
    = Hidden
    | Visible
    | Found


type alias Card =
    { id : Int
    , pokemonId : Int
    , state : CardState
    }


type alias Model =
    { gridReadyPokemonList : List Card, lastTick : Posix, nextUpdateTick : Posix, shouldUpdate : Bool }


init : ( Model, Cmd Msg )
init =
    ( { gridReadyPokemonList = []
      , lastTick = Time.millisToPosix 0
      , nextUpdateTick = Time.millisToPosix 0
      , shouldUpdate = False
      }
    , Random.generate Shuffled (Random.List.shuffle fullListOfPokemons)
    )


fullListOfPokemons : List Int
fullListOfPokemons =
    List.range 1 numberOfPokemons


listOfIntsToString : List Int -> String
listOfIntsToString myList =
    List.map (\i -> String.fromInt i ++ ",") myList
        |> String.concat


repeat : List a -> List a
repeat theList =
    theList ++ theList



---- UPDATE ----


type Msg
    = NoOp
    | ShuffleClicked
    | Shuffled (List Int)
    | ShuffledFullList (List Int)
    | PokemonCardClicked Int
    | TriggerCardChecks
    | NewFrame Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleClicked ->
            ( model, Random.generate Shuffled (Random.List.shuffle fullListOfPokemons) )

        Shuffled newList ->
            ( model
            , Random.generate ShuffledFullList
                (Random.List.shuffle <|
                    repeat
                        (newList |> List.take numberOfPokemonsNeeded)
                )
            )

        ShuffledFullList fullList ->
            ( { model
                | gridReadyPokemonList = List.indexedMap (\index theId -> { id = index, pokemonId = theId, state = Hidden }) fullList
              }
            , Cmd.none
            )

        PokemonCardClicked cardId ->
            let
                newSetOfCards =
                    flipCardWithId cardId model.gridReadyPokemonList

                newUpdate =
                    numberOfCardsFlipped newSetOfCards == 2
            in
            ( { model | gridReadyPokemonList = newSetOfCards, nextUpdateTick = addToPosix model.lastTick 500, shouldUpdate = newUpdate }, Cmd.none )

        TriggerCardChecks ->
            ( { model | gridReadyPokemonList = disableIfTWoFlipped model.gridReadyPokemonList }, Cmd.none )

        NewFrame tick ->
            if model.shouldUpdate && (Time.posixToMillis model.nextUpdateTick < Time.posixToMillis tick) then
                ( { model | lastTick = tick, shouldUpdate = False }, run TriggerCardChecks )

            else
                ( { model | lastTick = tick }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


addToPosix : Posix -> Int -> Posix
addToPosix current toAdd =
    Time.posixToMillis current
        + toAdd
        |> Time.millisToPosix


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


numberOfCardsFlipped : List Card -> Int
numberOfCardsFlipped cards =
    List.foldl
        (\c curr ->
            if c.state == Visible then
                curr + 1

            else
                curr
        )
        0
        cards


isCardVisible : Card -> Bool
isCardVisible card =
    card.state == Visible


setCardState : CardState -> Card -> Card
setCardState newState card =
    { card | state = newState }


checkVisibleCardsHaveSameId : List Card -> Bool
checkVisibleCardsHaveSameId cards =
    List.filter
        isCardVisible
        cards
        |> List.map
            (\c -> c.pokemonId)
        |> List.Extra.allDifferent
        |> not


disableIfTWoFlipped : List Card -> List Card
disableIfTWoFlipped cards =
    if numberOfCardsFlipped cards == 2 then
        if checkVisibleCardsHaveSameId cards then
            List.Extra.updateIf
                isCardVisible
                (setCardState Found)
                cards

        else
            List.Extra.updateIf
                isCardVisible
                (setCardState Hidden)
                cards

    else
        cards


flipCardWithId : Int -> List Card -> List Card
flipCardWithId cardId cards =
    List.Extra.updateIf
        (\c -> c.id == cardId)
        (\c -> { c | state = flipState c.state })
        cards


flipState : CardState -> CardState
flipState myState =
    case myState of
        Visible ->
            Hidden

        Hidden ->
            Visible

        Found ->
            Found



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ Element.width Element.fill, Element.height Element.fill ]
        (Element.column [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.row
                [ Element.width Element.fill
                , Element.height <|
                    Element.px 50
                , Element.Background.color redPokemon
                ]
                [ Element.image
                    [ Element.height <| Element.px 30
                    , Element.paddingEach
                        { top = 0
                        , right = 0
                        , bottom = 0
                        , left = 10
                        }
                    ]
                    { src = "logo-yellow.svg"
                    , description = "The Elm Logo"
                    }
                , Element.el
                    [ Element.paddingEach
                        { top = 0
                        , right = 0
                        , bottom = 0
                        , left = 10
                        }
                    , Element.Font.color yellowPokemon
                    , Element.Font.size 26
                    , Element.Font.family [ Element.Font.typeface "PokemonHollow" ]
                    ]
                  <|
                    Element.text "PokeMemory"
                ]
            , Element.column [ Element.width Element.fill, Element.height Element.fill ]
                [ Element.row [ Element.height Element.fill, Element.width Element.fill ]
                    (model.gridReadyPokemonList
                        |> List.map
                            (\card ->
                                Element.el [ Element.height <| Element.fillPortion 1, Element.width <| Element.fillPortion 1 ] <|
                                    Element.Input.button [ Element.height Element.fill, Element.width Element.fill ]
                                        { onPress =
                                            if model.shouldUpdate then
                                                Maybe.Nothing

                                            else
                                                Just <| PokemonCardClicked card.id
                                        , label =
                                            Element.image
                                                [ -- Element.width <| Element.px 30
                                                  -- , Element.height <| Element.px 30
                                                  Element.height Element.fill
                                                , Element.width Element.fill
                                                ]
                                                { src =
                                                    case card.state of
                                                        Visible ->
                                                            "pokemons/" ++ String.fromInt card.pokemonId ++ ".png"

                                                        Hidden ->
                                                            "pokemons/pokeball.png"

                                                        Found ->
                                                            "pokemons/found.png"
                                                , description = "The image of a pokemon"
                                                }
                                        }
                            )
                        |> groupsOf 4
                        |> List.map (\subList -> Element.column [] subList)
                    )
                , Element.row
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.Background.color whitePokemon
                    ]
                    [ Element.Input.button []
                        { onPress = Just ShuffleClicked
                        , label = Element.text "Shuffle List"
                        }
                    ]
                ]
            , Element.row
                [ Element.width Element.fill
                , Element.height <|
                    Element.px 50
                , Element.Background.color redPokemon
                , Element.alignBottom
                ]
                [ Element.el
                    [ Element.Font.color yellowPokemon
                    , Element.Font.family [ Element.Font.typeface "Titillium" ]
                    , Element.Font.size 16
                    , Element.width Element.fill
                    , Element.Font.center
                    ]
                  <|
                    Element.text "Created in Elm by Julien Lengrand-Lambert on Twitch, live"
                ]
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame NewFrame



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
