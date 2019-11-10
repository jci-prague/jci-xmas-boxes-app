module Views exposing
    ( filterFormView
    , reservationFormView
    , viewFamilies
    )

import Html
    exposing
        ( Html
        , a
        , button
        , div
        , input
        , label
        , span
        , text
        )
import Html.Attributes
    exposing
        ( class
        , disabled
        , for
        , href
        , name
        , placeholder
        , type_
        )
import Html.Events exposing (onClick, onInput)
import Types
    exposing
        ( Child
        , ChildList
        , Family
        , FamilyId(..)
        , Gender(..)
        , Model
        , Msg(..)
        )


filterFormView : Model -> Html Msg
filterFormView model =
    div []
        [ div [ class "row form-group" ]
            [ span [ class "col" ] [ text "Od" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 1), onClick (SetBottomThreshold 1) ] [ text "1" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 2), onClick (SetBottomThreshold 2) ] [ text "2" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 3), onClick (SetBottomThreshold 3) ] [ text "3" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 4), onClick (SetBottomThreshold 4) ] [ text "4" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 5), onClick (SetBottomThreshold 5) ] [ text "5" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 6), onClick (SetBottomThreshold 6) ] [ text "6" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 7), onClick (SetBottomThreshold 7) ] [ text "7" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 8), onClick (SetBottomThreshold 8) ] [ text "8" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 9), onClick (SetBottomThreshold 9) ] [ text "9" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 10), onClick (SetBottomThreshold 10) ] [ text "10" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 11), onClick (SetBottomThreshold 11) ] [ text "11" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 12), onClick (SetBottomThreshold 12) ] [ text "12" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 13), onClick (SetBottomThreshold 13) ] [ text "13" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 14), onClick (SetBottomThreshold 14) ] [ text "14" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 15), onClick (SetBottomThreshold 15) ] [ text "15" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.bottomThreshold 16), onClick (SetBottomThreshold 16) ] [ text "16" ]
            ]
        , div [ class "row form-group" ]
            [ span [ class "col" ] [ text "Do" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 2), onClick (SetTopThreshold 2) ] [ text "2" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 3), onClick (SetTopThreshold 3) ] [ text "3" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 4), onClick (SetTopThreshold 4) ] [ text "4" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 5), onClick (SetTopThreshold 5) ] [ text "5" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 6), onClick (SetTopThreshold 6) ] [ text "6" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 7), onClick (SetTopThreshold 7) ] [ text "7" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 8), onClick (SetTopThreshold 8) ] [ text "8" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 9), onClick (SetTopThreshold 9) ] [ text "9" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 10), onClick (SetTopThreshold 10) ] [ text "10" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 11), onClick (SetTopThreshold 11) ] [ text "11" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 12), onClick (SetTopThreshold 12) ] [ text "12" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 13), onClick (SetTopThreshold 13) ] [ text "13" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 14), onClick (SetTopThreshold 14) ] [ text "14" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 15), onClick (SetTopThreshold 15) ] [ text "15" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 16), onClick (SetTopThreshold 16) ] [ text "16" ]
            , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 17), onClick (SetTopThreshold 17) ] [ text "17" ]
            ]
        , div [ class "row form-group" ]
            [ span [ class "col" ] [ text "Pohlaví" ]
            , button [ class ("col btn " ++ isGenderEqualSelected model.selectedGender Male), onClick (SetGender Male) ] [ text "Kluk" ]
            , button [ class ("col btn " ++ isGenderEqualSelected model.selectedGender Female), onClick (SetGender Female) ] [ text "Holka" ]
            , button [ class ("col btn " ++ isGenderEqualSelected model.selectedGender NotImportant), onClick (SetGender NotImportant) ] [ text "Nezáleží" ]
            ]
        ]


viewFamilies : Model -> Html Msg
viewFamilies model =
    let
        familyContent =
            if List.length model.families > 0 then
                if List.length model.viewableFamilies > 0 then
                    div [ class "col-9 margin-bottom-1em" ]
                        (List.map
                            (\f ->
                                viewFamily f
                            )
                            model.viewableFamilies
                        )

                else
                    div [ class "col-9 alert alert-primary" ] [ text "Zvolenému filtru již neodpovídá žádné dítě, zkuste upravit věk nebo pohlaví." ]

            else
                div [ class "col-9 alert alert-primary" ] [ text "Bohužel, momentálně nejsou k dispozici žádné děti k obdarování." ]
    in
    div [ class "row" ]
        [ div [ class "col" ] [ text "Děti či sourozenci" ]
        , familyContent
        ]


reservationFormView : Model -> Html Msg
reservationFormView model =
    div [ class "row" ]
        [ div [ class "col-sm" ]
            [ div [ class "row form-group" ]
                [ label [ class "col", for "name" ] [ text "Jméno" ]
                , input [ class "col-9 input-control", type_ "text", name "name", placeholder "Jméno", onInput UpdateName ] []
                ]
            , div [ class "row form-group" ]
                [ label [ class "col", for "email" ] [ text "Email" ]
                , input [ class "col-9 input-control", type_ "text", name "email", placeholder "jirka@seznam.cz", onInput UpdateEmail ] []
                ]
            , div [ class "row" ]
                [ span [ class "col" ] [ text "Vybrané děti" ]
                , div [ class "col-9" ] [ viewSelectedFamilies model ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "" ]
                , label [ class "col-9 form-check-label" ]
                    [ input [ type_ "checkbox", onClick ToggleAgreement ] []
                    , span []
                        [ text "  Souhlasím se "
                        , a [ href "/gdpr/" ] [ text "zpracováním poskytnutách osobních údajů" ]
                        , text "."
                        ]
                    ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "" ]
                , button [ class "col-9 btn btn-primary", onClick SendReservation, disabled (not model.agreement) ] [ text "Zaregistrovat se" ]
                ]
            , div [ class "row" ]
                [ div [ class "col-3" ] []
                , viewMessagePane model
                ]
            ]
        ]


viewMessagePane : Model -> Html Msg
viewMessagePane model =
    case model.errorMessage of
        Just errMessage ->
            div [ class "col-9 alert alert-danger" ] [ text errMessage ]

        Nothing ->
            case model.successMessage of
                Just succMessage ->
                    div [ class "col-9 alert alert-success" ] [ text succMessage ]

                Nothing ->
                    div [] []


isButtonEqualTreshold : Int -> Int -> String
isButtonEqualTreshold currentTreshold buttonTreshold =
    if currentTreshold == buttonTreshold then
        "btn-primary"

    else
        "btn-outline-primary"


isGenderEqualSelected : Gender -> Gender -> String
isGenderEqualSelected selectedGender buttonGender =
    if selectedGender == buttonGender then
        "btn-primary"

    else
        "btn-outline-primary"


viewSelectedFamilies : Model -> Html Msg
viewSelectedFamilies model =
    if List.length model.selectedFamilies > 0 then
        div [ class "row margin-bottom-1em" ]
            [ div [ class "col" ]
                (List.map
                    (\f ->
                        viewSelectedFamily f
                    )
                    model.selectedFamilies
                )
            ]

    else
        div [ class "row margin-bottom-1em" ]
            [ div [ class "col alert alert-primary" ]
                [ text "Nemáte vybrané ještě žádné děti k obdarování."
                ]
            ]


viewSelectedFamily : Family -> Html Msg
viewSelectedFamily family =
    let
        firstChild : Child
        firstChild =
            case List.head family.children of
                Just child ->
                    child

                Nothing ->
                    Child "" 0 NotImportant

        otherChildren : ChildList
        otherChildren =
            case List.tail family.children of
                Just children ->
                    children

                Nothing ->
                    []

        childRows =
            div [ class "row margin-top-1-5em" ]
                [ span [ class "col" ] [ text firstChild.name ]
                , span [ class "col" ] [ text (String.fromInt firstChild.age) ]
                , button [ class "col btn btn-danger", onClick (RemoveFamilyFromSelected family.familyId) ] [ text "Odebrat" ]
                ]
                :: List.map
                    (\child ->
                        div [ class "row" ]
                            [ span [ class "col" ] [ text child.name ]
                            , span [ class "col" ] [ text (String.fromInt child.age) ]
                            , span [ class "col" ] [ text "" ]
                            ]
                    )
                    otherChildren
    in
    div [] childRows


viewFamily : Family -> Html Msg
viewFamily family =
    let
        firstChild : Child
        firstChild =
            case List.head family.children of
                Just child ->
                    child

                Nothing ->
                    Child "" 0 NotImportant

        otherChildren : ChildList
        otherChildren =
            case List.tail family.children of
                Just children ->
                    children

                Nothing ->
                    []

        childRows =
            div [ class "row margin-top-1-5em" ]
                [ span [ class "col" ] [ text firstChild.name ]
                , span [ class "col" ] [ text (String.fromInt firstChild.age) ]
                , button [ class "col btn btn-primary", onClick (AddFamilyToSelected family.familyId) ] [ text "Obdarovat" ]
                ]
                :: List.map
                    (\child ->
                        div [ class "row" ]
                            [ span [ class "col" ] [ text child.name ]
                            , span [ class "col" ] [ text (String.fromInt child.age) ]
                            , span [ class "col" ] [ text "" ]
                            ]
                    )
                    otherChildren
    in
    div [] childRows
