module AdminMain exposing (main)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    Int


type Msg
    = None


type alias Flags =
    { variable : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "Elm App works!" ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = initSubscriptions
        }


initSubscriptions : Model -> Sub Msg
initSubscriptions _ =
    Sub.none


initialModel : Flags -> ( Model, Cmd Msg )
initialModel _ =
    ( 0, Cmd.none )
