module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (Element, el)
import Element.Input
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Random
import Random.List


---- MODEL ----

numberOfRows : Int
numberOfRows = 4

numberOfColumns : Int
numberOfColumns = 4

numberOfPokemonsNeeded : Int
numberOfPokemonsNeeded = (numberOfColumns * numberOfRows) // 2

numberOfPokemons : Int
numberOfPokemons = 13

type CardState =
    Hidden
    | Visible

type alias Card = {
    id : Int, 
    state : CardState}

type alias Model =
    {gridReadyPokemonList : List Card}


init : ( Model, Cmd Msg )
init =
    ( {
        gridReadyPokemonList = []
    }, Random.generate Shuffled (Random.List.shuffle fullListOfPokemons) )

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ShuffleClicked ->
            ( model, Random.generate Shuffled (Random.List.shuffle fullListOfPokemons) )
        Shuffled newList ->
                (model
                , Random.generate ShuffledFullList (
                    Random.List.shuffle <| repeat
                 (newList |> List.take numberOfPokemonsNeeded)   
                ))
        
        ShuffledFullList fullList ->
            let
                cardList = List.map (\theId -> {id = theId, state = Visible}) fullList 
            in 
            ({model | gridReadyPokemonList = cardList}, Cmd.none)
        NoOp -> 
            ( model, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [] (
        Element.column [] [
            Element.image [Element.width <| Element.px 300, Element.height <| Element.px 300] {
                src= "logo.svg",
                description= "The Elm Logo"
            }
            , Element.text "Our memory game" 
            , Element.row [] 
                (model.gridReadyPokemonList
                    |> List.map (\card -> 
                        Element.image [Element.width <| Element.px 30, Element.height <| Element.px 30]{
                            src = "pokemons/" ++ String.fromInt card.id ++ ".png"
                            , description = "The image of a pokemon"
                        }
                    )
                )
            , Element.Input.button [] {
                onPress = Just ShuffleClicked
                ,label = Element.text "Shuffle List"
            }
        ]
    )


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
