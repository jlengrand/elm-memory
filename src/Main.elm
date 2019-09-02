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


numberColumns : Int
numberColumns =
    4


numberRows : Int
numberRows =
    4


numberOfImages : Int
numberOfImages =
    13


takeImagesFromCatalog : Int -> Int -> List Int
takeImagesFromCatalog sampleSize totalSize =
    List.range 1 totalSize 


intListToString : List Int -> String
intListToString myList =
    String.concat <| List.map (\s -> String.fromInt s ++ ",") myList



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout []
        (Element.column
            [ Element.spacing 40 ]
            [ Element.image
                [ Element.width <| Element.px 300
                , Element.height <| Element.px 300
                , Element.padding 20
                ]
                { src = "logo.svg", description = "Elm logo" }
            , Element.text "Your Elm App is working!"
            , Element.text <| intListToString <| takeImagesFromCatalog 3 13
            , el []
                (Element.column []
                    (List.range 1 numberColumns
                        |> List.map
                            (\col ->
                                List.range 1 numberRows
                                    |> List.map
                                        (\row -> Element.text (String.fromInt row ++ String.fromInt col))
                            )
                        |> List.concat
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
