module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (..)


type Gender
    = Male
    | Female


type alias Child =
    { name : String
    , age : Int
    , gender : Gender
    }


type alias Family =
    { children : List Child
    }


type alias Model =
    { families : List Family
    }


type Msg
    = None


view : Model -> Html Msg
view model =
    div []
        [ ul []
            (List.map
                (\f ->
                    (li []
                        [ (viewFamily f) ]
                    )
                )
                model.families
            )
        ]


viewFamily : Family -> Html Msg
viewFamily family =
    div []
        (List.map
            (\ch -> div [] [ (text ch.name) ])
            family.children
        )


initialModel : Model
initialModel =
    { families =
        [ familyJimm
        , familyEmaJohny
        , familyMary
        ]
    }


update : Msg -> (Model -> Model)
update msg model =
    case msg of
        None ->
            model


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


childEma : Child
childEma =
    { name = "Ema"
    , age = 3
    , gender = Female
    }


childJohny : Child
childJohny =
    { name = "Johny"
    , age = 5
    , gender = Male
    }


familyEmaJohny : Family
familyEmaJohny =
    { children = [ childEma, childJohny ]
    }


familyJimm : Family
familyJimm =
    { children =
        [ { name = "Jimm", age = 11, gender = Male }
        ]
    }


familyMary : Family
familyMary =
    { children =
        [ { name = "Mary", age = 8, gender = Female }
        ]
    }
