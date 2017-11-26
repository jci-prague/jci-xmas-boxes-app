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
    , viewvableFamilies : List Family
    , bottomThreshold: Int
    , topThreshold: Int
    }


type Msg
    = SetBottomThreshold Int
    | SetTopThreshold Int
    | None


view : Model -> Html Msg
view model =
  div []
      [ (filterFormView model)
      , (viewFamilies model)
      ]


filterFormView : Model -> Html Msg
filterFormView model =
  div [] 
      [ span [] [text "Věk od"]
      , button [ onClick (SetBottomThreshold  1) ] [ text  "1" ]
      , button [ onClick (SetBottomThreshold  2) ] [ text  "2" ]
      , button [ onClick (SetBottomThreshold  3) ] [ text  "3" ]
      , button [ onClick (SetBottomThreshold  4) ] [ text  "4" ]
      , button [ onClick (SetBottomThreshold  5) ] [ text  "5" ]
      , button [ onClick (SetBottomThreshold  6) ] [ text  "6" ]
      , button [ onClick (SetBottomThreshold  7) ] [ text  "7" ]
      , button [ onClick (SetBottomThreshold  8) ] [ text  "8" ]
      , button [ onClick (SetBottomThreshold  9) ] [ text  "9" ]
      , button [ onClick (SetBottomThreshold 10) ] [ text "10" ]
      , button [ onClick (SetBottomThreshold 11) ] [ text "11" ]
      , button [ onClick (SetBottomThreshold 12) ] [ text "12" ]
      , button [ onClick (SetBottomThreshold 13) ] [ text "13" ]
      , button [ onClick (SetBottomThreshold 14) ] [ text "14" ]
      , button [ onClick (SetBottomThreshold 15) ] [ text "15" ]
      , button [ onClick (SetBottomThreshold 16) ] [ text "16" ]
      , span []
        [ (text ((toString model.bottomThreshold) ++ " - " ++ (toString model.topThreshold)))
        ]
      , span [] [text "Věk do"]
      , button [ onClick (SetTopThreshold  2) ] [ text  "2" ]
      , button [ onClick (SetTopThreshold  3) ] [ text  "3" ]
      , button [ onClick (SetTopThreshold  4) ] [ text  "4" ]
      , button [ onClick (SetTopThreshold  5) ] [ text  "5" ]
      , button [ onClick (SetTopThreshold  6) ] [ text  "6" ]
      , button [ onClick (SetTopThreshold  7) ] [ text  "7" ]
      , button [ onClick (SetTopThreshold  8) ] [ text  "8" ]
      , button [ onClick (SetTopThreshold  9) ] [ text  "9" ]
      , button [ onClick (SetTopThreshold 10) ] [ text "10" ]
      , button [ onClick (SetTopThreshold 11) ] [ text "11" ]
      , button [ onClick (SetTopThreshold 12) ] [ text "12" ]
      , button [ onClick (SetTopThreshold 13) ] [ text "13" ]
      , button [ onClick (SetTopThreshold 14) ] [ text "14" ]
      , button [ onClick (SetTopThreshold 15) ] [ text "15" ]
      , button [ onClick (SetTopThreshold 16) ] [ text "16" ]
      , button [ onClick (SetTopThreshold 17) ] [ text "17" ]
      ]


viewFamilies : Model -> Html Msg
viewFamilies model =
    div []
        [ ul []
            (List.map
                (\f ->
                    (li []
                        [ (viewFamily f) ]
                    )
                )
                model.viewvableFamilies
            )
        ]


viewFamily : Family -> Html Msg
viewFamily family =
    div []
        (List.map
            (\ch -> div [ class (classForGender ch) ]
                [ span [class "childName"] [(text ch.name)]
                , span [class "childAge"] [(text (toString ch.age))]
                ]
            )
            family.children
        )


classForGender : Child -> String
classForGender child =
    if child.gender == Male then
        "male"
    else 
        "female"


initialModel : Model
initialModel =
    { families = initialFamilies
    , viewvableFamilies = initialFamilies
    , bottomThreshold = 1
    , topThreshold = 17
    }


initialFamilies =
        [ familyJimm
        , familyEmaJohny
        , familyMary
        , familyJessie
        ]


update : Msg -> (Model -> Model)
update msg model =
    case msg of
        SetBottomThreshold threshold ->
            { model | bottomThreshold = threshold }
        SetTopThreshold threshold ->
            { model | topThreshold = threshold }
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

familyJessie : Family
familyJessie =
    { children =
        [ { name = "Jessie", age = 11, gender = Female }
        ]
    }
