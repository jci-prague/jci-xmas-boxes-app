module Main exposing (Model, addToSelectedFamilies, anyChildInAgeRangeAndGender, childEma, childJohny, classForGender, familyEmaJohny, familyJessie, familyJimm, familyMary, filterFormView, findFamilyById, initialFamilies, initialModel, main, removeSelectedFamilies, reservationFormView, update, updateViewableFamilies, view, viewFamilies, viewFamily, viewSelectedFamilies, viewSelectedFamily)

import Browser exposing (sandbox)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (..)
import Requests exposing (..)
import Types exposing (..)


type alias Model =
    { families : List Family
    , viewableFamilies : List Family
    , bottomThreshold : Int
    , topThreshold : Int
    , selectedGender : Gender
    , selectedFamilies : List Family
    }


update : Msg -> (Model -> Model)
update msg model =
    case msg of
        SetBottomThreshold threshold ->
            { model
                | bottomThreshold = threshold
                , viewableFamilies = updateViewableFamilies threshold model.topThreshold model.selectedGender model.families
            }

        SetTopThreshold threshold ->
            { model
                | topThreshold = threshold
                , viewableFamilies = updateViewableFamilies model.bottomThreshold threshold model.selectedGender model.families
            }

        SetGender gender ->
            { model
                | selectedGender = gender
                , viewableFamilies = updateViewableFamilies model.bottomThreshold model.topThreshold gender model.families
            }

        AddFamilyToSelected familyId ->
            { model
                | selectedFamilies = addToSelectedFamilies model familyId
                , viewableFamilies = removeSelectedFamilies model familyId
            }

        RemoveFamilyFromSelected familyId ->
            model

        SendReservation ->
            model

        None ->
            model


addToSelectedFamilies : Model -> FamilyId -> List Family
addToSelectedFamilies model familyId =
    findFamilyById model.viewableFamilies familyId :: model.selectedFamilies


removeSelectedFamilies : Model -> FamilyId -> List Family
removeSelectedFamilies model familyId =
    List.filter (\f -> familyId /= f.familyId) model.viewableFamilies


findFamilyById : List Family -> FamilyId -> Family
findFamilyById families familyId =
    let
        filteredFamilies =
            List.filter (\f -> familyId == f.familyId) families

        maybeFamily =
            List.head filteredFamilies
    in
    case maybeFamily of
        Just value ->
            value

        Nothing ->
            { familyId = familyId, children = [] }


updateViewableFamilies : Int -> Int -> Gender -> List Family -> List Family
updateViewableFamilies bottom top gender families =
    List.filter
        (\f -> anyChildInAgeRangeAndGender bottom top gender f)
        families


anyChildInAgeRangeAndGender : Int -> Int -> Gender -> Family -> Bool
anyChildInAgeRangeAndGender bottom top gender family =
    List.any
        (\child ->
            (child.age >= bottom && child.age <= top)
                && (gender == NotImportant || child.gender == gender)
        )
        family.children


view : Model -> Html Msg
view model =
    div []
        [ filterFormView model
        , viewSelectedFamilies model
        , reservationFormView model
        , viewFamilies model
        ]


reservationFormView : Model -> Html Msg
reservationFormView model =
    div []
        [ label [ for "name" ] [ text "Jméno" ]
        , input [ type_ "text", name "name", placeholder "Jméno" ] []
        , label [ for "email" ] [ text "Email" ]
        , input [ type_ "text", name "email", placeholder "jirka@seznam.cz" ] []
        , button [ onClick SendReservation ] [ text "Zaregistrovat se" ]
        ]


filterFormView : Model -> Html Msg
filterFormView model =
    div []
        [ div []
            [ span [] [ text "Věk: " ]
            , span []
                [ text (toString model.bottomThreshold ++ " - " ++ toString model.topThreshold)
                ]
            , span []
                [ text ("(" ++ toString model.selectedGender ++ ")")
                ]
            ]
        , div []
            [ span [] [ text "Od" ]
            , button [ onClick (SetBottomThreshold 1) ] [ text "1" ]
            , button [ onClick (SetBottomThreshold 2) ] [ text "2" ]
            , button [ onClick (SetBottomThreshold 3) ] [ text "3" ]
            , button [ onClick (SetBottomThreshold 4) ] [ text "4" ]
            , button [ onClick (SetBottomThreshold 5) ] [ text "5" ]
            , button [ onClick (SetBottomThreshold 6) ] [ text "6" ]
            , button [ onClick (SetBottomThreshold 7) ] [ text "7" ]
            , button [ onClick (SetBottomThreshold 8) ] [ text "8" ]
            , button [ onClick (SetBottomThreshold 9) ] [ text "9" ]
            , button [ onClick (SetBottomThreshold 10) ] [ text "10" ]
            , button [ onClick (SetBottomThreshold 11) ] [ text "11" ]
            , button [ onClick (SetBottomThreshold 12) ] [ text "12" ]
            , button [ onClick (SetBottomThreshold 13) ] [ text "13" ]
            , button [ onClick (SetBottomThreshold 14) ] [ text "14" ]
            , button [ onClick (SetBottomThreshold 15) ] [ text "15" ]
            , button [ onClick (SetBottomThreshold 16) ] [ text "16" ]
            ]
        , div []
            [ span [] [ text "Do" ]
            , button [ onClick (SetTopThreshold 2) ] [ text "2" ]
            , button [ onClick (SetTopThreshold 3) ] [ text "3" ]
            , button [ onClick (SetTopThreshold 4) ] [ text "4" ]
            , button [ onClick (SetTopThreshold 5) ] [ text "5" ]
            , button [ onClick (SetTopThreshold 6) ] [ text "6" ]
            , button [ onClick (SetTopThreshold 7) ] [ text "7" ]
            , button [ onClick (SetTopThreshold 8) ] [ text "8" ]
            , button [ onClick (SetTopThreshold 9) ] [ text "9" ]
            , button [ onClick (SetTopThreshold 10) ] [ text "10" ]
            , button [ onClick (SetTopThreshold 11) ] [ text "11" ]
            , button [ onClick (SetTopThreshold 12) ] [ text "12" ]
            , button [ onClick (SetTopThreshold 13) ] [ text "13" ]
            , button [ onClick (SetTopThreshold 14) ] [ text "14" ]
            , button [ onClick (SetTopThreshold 15) ] [ text "15" ]
            , button [ onClick (SetTopThreshold 16) ] [ text "16" ]
            , button [ onClick (SetTopThreshold 17) ] [ text "17" ]
            ]
        , div []
            [ span [] [ text "Pohlaví" ]
            , button [ onClick (SetGender Male) ] [ text "Kluk" ]
            , button [ onClick (SetGender Female) ] [ text "Holka" ]
            , button [ onClick (SetGender NotImportant) ] [ text "Nezáleží" ]
            ]
        ]


viewSelectedFamilies : Model -> Html Msg
viewSelectedFamilies model =
    div []
        [ ul []
            (List.map
                (\f ->
                    li []
                        [ viewSelectedFamily f ]
                )
                model.selectedFamilies
            )
        ]


viewFamilies : Model -> Html Msg
viewFamilies model =
    div []
        [ ul []
            (List.map
                (\f ->
                    li []
                        [ viewFamily f ]
                )
                model.viewableFamilies
            )
        ]


viewSelectedFamily : Family -> Html Msg
viewSelectedFamily family =
    div []
        (List.map
            (\ch ->
                div [ class (classForGender ch) ]
                    [ span [ class "childName" ] [ text ch.name ]
                    , span [ class "childAge" ] [ text (toString ch.age) ]
                    , button [ onClick (RemoveFamilyFromSelected family.familyId) ] [ text "Odebrat" ]
                    ]
            )
            family.children
        )


viewFamily : Family -> Html Msg
viewFamily family =
    div []
        (List.map
            (\ch ->
                div [ class (classForGender ch) ]
                    [ span [ class "childName" ] [ text ch.name ]
                    , span [ class "childAge" ] [ text (toString ch.age) ]
                    , button [ onClick (AddFamilyToSelected family.familyId) ] [ text "Obdarovat" ]
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
    , viewableFamilies = initialFamilies
    , bottomThreshold = 1
    , topThreshold = 17
    , selectedGender = NotImportant
    , selectedFamilies = []
    }


initialFamilies =
    [ familyJimm
    , familyEmaJohny
    , familyMary
    , familyJessie
    ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
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
    { familyId = FamilyId "F1001"
    , children = [ childEma, childJohny ]
    }


familyJimm : Family
familyJimm =
    { familyId = FamilyId "F1002"
    , children =
        [ { name = "Jimm", age = 11, gender = Male }
        ]
    }


familyMary : Family
familyMary =
    { familyId = FamilyId "F1003"
    , children =
        [ { name = "Mary", age = 8, gender = Female }
        ]
    }


familyJessie : Family
familyJessie =
    { familyId = FamilyId "F1004"
    , children =
        [ { name = "Jessie", age = 11, gender = Female }
        ]
    }
