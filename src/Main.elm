module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (Element, el)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                (List.range 1 4
                    |> List.map (\columnNumber ->
                        Element.column [] ( 
                        List.range 1 4 
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
