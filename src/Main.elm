module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----

type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )

numberColumns : Int
numberColumns = 3

numberRows : Int
numberRows = 3


---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div []
            (List.range 1 numberColumns
                |> List.map
                    (\col ->
                        List.range 1 numberRows
                            |> List.map (\row -> div [] [ text (String.fromInt row ++ String.fromInt col) ])
                    )
                |> List.concat
            )
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
