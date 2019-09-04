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
numberOfRows = 3

numberOfColumns : Int
numberOfColumns = 3

numberOfPokemonsNeeded : Int
numberOfPokemonsNeeded = 8

numberOfPokemons : Int
numberOfPokemons = 13

type alias Model =
    {selectedPokemonList : List Int}


init : ( Model, Cmd Msg )
init =
    ( {
        selectedPokemonList = []
    }, Cmd.none )

fullListOfPokemons : List Int
fullListOfPokemons =
    List.range 1 numberOfPokemons

listOfIntsToString : List Int -> String
listOfIntsToString myList =
    List.map (\i -> String.fromInt i ++ ",") myList
    |> String.concat

---- UPDATE ----


type Msg
    = NoOp
    | ShuffleClicked
    | Shuffled (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ShuffleClicked ->
            ( model, Random.generate Shuffled (Random.List.shuffle fullListOfPokemons) )
        Shuffled newList ->
            ({selectedPokemonList = newList |> List.take numberOfPokemonsNeeded}, Cmd.none)
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
                (List.range 1 (numberOfColumns + 1)
                    |> List.map (\columnNumber ->
                        Element.column [] ( 
                        List.range 1 (numberOfRows + 1)
                            |> List.map (\rowNumber -> 
                                    Element.row [] [
                                        Element.image [Element.width <| Element.px 30, Element.height <| Element.px 30]{
                                            src = "pokemons/1.png"
                                            , description = "The image of a pokemon"
                                        }
                                        -- ,Element.text <| "[" ++ String.fromInt rowNumber ++ String.fromInt columnNumber ++ "]"
                                    ]
                                )
                        )
                    )
                )
            , Element.text <| listOfIntsToString fullListOfPokemons
            , Element.text <| listOfIntsToString model.selectedPokemonList
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
