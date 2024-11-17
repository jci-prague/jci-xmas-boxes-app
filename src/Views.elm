module Views exposing
    ( filterFormView
    , reservationFormView
    , viewFamilies
    )

-- import GlobalCenter

import Center
    exposing
        ( CenterId(..)
        , CenterList
        , findCenterByCenterId
        , findGlobalUniversalCenter
        , unpackCenterId
        )
import GlobalCenter exposing (GlobalCenter)
import Html
    exposing
        ( Html
        , a
        , b
        , button
        , div
        , i
        , input
        , label
        , option
        , select
        , span
        , text
        )
import Html.Attributes
    exposing
        ( checked
        , class
        , disabled
        , for
        , href
        , name
        , placeholder
        , selected
        , target
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Place
    exposing
        ( PlaceList
        )
import Types
    exposing
        ( AppState(..)
        , Child
        , ChildList
        , Family
        , FamilyId(..)
        , FamilyList
        , Gender(..)
        , Model
        , Msg(..)
        , unpackFamilyId
        )


filterFormView : Model -> Html Msg
filterFormView model =
    if model.appState == Start then
        div []
            [ div [ class "row form-group" ]
                (viewPlacesToggleButtons model.families model.places)
            ]

    else
        div []
            [ div [ class "row form-group" ]
                [ div [ class "col-2" ] [ text "Vybrané město" ]
                , div [ class "col-10" ] [ viewActivePlace model ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "Věk od" ]
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
                [ span [ class "col" ] [ text "Věk do" ]
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
                , button [ class ("col btn " ++ isButtonEqualTreshold model.topThreshold 17), onClick (SetTopThreshold 17) ] [ text "17+" ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "Pohlaví" ]
                , button [ class ("col btn " ++ isGenderEqualSelectedClass model.selectedGender Male), onClick (SetGender Male) ]
                    [ isGenderEqualSelectedIcon model.selectedGender Male
                    , text " Kluk"
                    ]
                , button [ class ("col btn " ++ isGenderEqualSelectedClass model.selectedGender Female), onClick (SetGender Female) ]
                    [ isGenderEqualSelectedIcon model.selectedGender Female
                    , text " Holka"
                    ]
                , button [ class ("col btn " ++ isGenderEqualSelectedClass model.selectedGender NotImportant), onClick (SetGender NotImportant) ]
                    [ isGenderEqualSelectedIcon model.selectedGender NotImportant
                    , text " Nezáleží"
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ] [ text "Můžeš obdarovat jedno dítě, sourozence (skupina dětí se společným tlačítkem 'Vybrat') nebo i více dětí najednou. Tvoje vybrané děti se objevují ve formuláři pod seznamem. Po kliknutí na tlačítko VYBRAT, odskroluj až úplně dolů a dokonči proces vyplněním formuláře a klinutím na tlačítko OBDAROVAT. Po úspěšné odeslání Ti bude obratem zaslán e-mail se všemi informacemi." ]
                ]
            ]


viewPlacesToggleButtons : FamilyList -> PlaceList -> List (Html Msg)
viewPlacesToggleButtons families places =
    let
        rowLabel =
            span [ class "col" ] [ text "Město" ]

        placeToggleButtons =
            places
                |> List.filter
                    (\place ->
                        let
                            countFamilies =
                                families
                                    |> List.filter (\family -> family.placeId == place.placeId)
                                    |> List.length
                        in
                        (countFamilies > 0) && place.available
                    )
                |> List.map
                    (\place ->
                        let
                            btnState =
                                if place.active then
                                    "btn-primary city-btn"

                                else
                                    "btn-outline-primary city-btn"
                        in
                        button [ class ("col btn " ++ btnState), onClick (PlaceToggle place.placeId) ]
                            [ if place.active then
                                i [ class "fa fa-check-circle" ] []

                              else
                                i [ class "fa fa-ban" ] []
                            , text (" " ++ place.name)
                            ]
                    )
    in
    rowLabel :: placeToggleButtons


viewFamilies : Model -> Html Msg
viewFamilies model =
    let
        familyContent =
            if List.length model.families > 0 then
                if List.length model.viewableFamilies > 0 then
                    div [ class "col-sm-12 col-md-9 margin-bottom-1em" ]
                        (div [ class "row margin-top-1-5em family-list-header" ]
                            [ span [ class "col-sm-2" ] [ text "Jméno" ]
                            , span [ class "col-sm-1" ] [ text "Věk" ]
                            , span [ class "col-sm-7" ] [ text "Přání/záliby" ]
                            , span [ class "col-sm-2" ] [ text "" ]
                            ]
                            :: List.map
                                (\f ->
                                    viewFamily f
                                )
                                model.viewableFamilies
                        )

                else
                    div [ class "col-9 alert alert-primary" ] [ text "Zvolenému filtru již neodpovídá žádné dítě, zkuste upravit věk nebo pohlaví." ]

            else
                div [ class "col-9 alert alert-primary" ] [ text "Pro vybrané město nejsou k dispozici žádné děti k obdarování. Prosím, vyberte nebo změňte vybrané město." ]
    in
    div [ class "row margin-top-1-5em" ]
        [ div [ class "col" ] [ text "Děti či sourozenci" ]
        , familyContent
        ]


reservationFormView : Model -> Html Msg
reservationFormView model =
    div [ class "row" ]
        [ div [ class "col-sm" ]
            [ div [ class "row form-group" ]
                [ label [ class "col", for "name" ] [ text "Jméno" ]
                , input [ class "col-9 input-control", type_ "text", name "name", placeholder "Jméno", value (Maybe.withDefault "" model.donorName), onInput UpdateName ] []
                ]
            , div [ class "row form-group" ]
                [ label [ class "col", for "email" ] [ text "Email" ]
                , input [ class "col-9 input-control", type_ "text", name "email", placeholder "jirka@seznam.cz", value (Maybe.withDefault "" model.donorEmail), onInput UpdateEmail ] []
                ]
            , div [ class "row form-group" ]
                [ label [ class "col-3", for "email2" ] [ text "Zopakovat email" ]
                , input [ class "col-9 input-control", type_ "text", name "email2", placeholder "jirka@seznam.cz", value (Maybe.withDefault "" model.donorEmail2), onInput UpdateEmail2 ] []
                , span [ class "col-3" ] []
                , case model.donorEmailErrorMessage of
                    Just message ->
                        span [ class "col-9 text-danger mt-1" ] [ text message ]

                    Nothing ->
                        span [ class "col-9 mt-1" ] [ text "" ]
                ]
            , div [ class "row" ]
                [ span [ class "col" ] [ text "Vybrané děti" ]
                , div [ class "col-9" ] [ viewSelectedFamilies model ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "" ]
                , label [ class "col-9 form-check-label" ]
                    [ input [ type_ "checkbox", checked model.agreement, onClick ToggleAgreement ] []
                    , span []
                        [ text "  Souhlasím se "
                        , a [ href "/gdpr/", target "_blank" ] [ text "zpracováním poskytnutých osobních údajů" ]
                        , text "."
                        ]
                    ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "" ]
                , button [ class "col-9 btn btn-primary", onClick SendReservation, disabled (not model.agreement) ] [ text "Obdarovat" ]
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


isGenderEqualSelected : Gender -> Gender -> Bool
isGenderEqualSelected selectedGender buttonGender =
    if selectedGender == buttonGender then
        True

    else
        False


isGenderEqualSelectedClass : Gender -> Gender -> String
isGenderEqualSelectedClass selectedGender buttonGender =
    if isGenderEqualSelected selectedGender buttonGender then
        "btn-primary"

    else
        "btn-outline-primary"


isGenderEqualSelectedIcon : Gender -> Gender -> Html msg
isGenderEqualSelectedIcon selectedGender buttonGender =
    if isGenderEqualSelected selectedGender buttonGender then
        i [ class "fa fa-check-circle" ] []

    else
        i [ class "fa fa-ban" ] []


viewSelectedFamilies : Model -> Html Msg
viewSelectedFamilies model =
    if List.length model.selectedFamilies > 0 then
        div [ class "container margin-bottom-1em" ]
            [ div [ class "" ]
                (List.map
                    (\f ->
                        viewSelectedFamily model.centers f
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


viewSelectedFamily : CenterList -> Family -> Html Msg
viewSelectedFamily centers family =
    let
        globalCenter =
            findGlobalUniversalCenter centers

        childRows =
            let
                familyCenterControls : Family -> Html Msg
                familyCenterControls fam =
                    div [ class "row form-group" ]
                        [ label [ class "col-sm-3 col-form-label" ] [ text "Sběrné Místo" ]
                        , if fam.centerId == globalCenter.centerId then
                            span [ class "col-sm-9" ] [ text globalCenter.name ]

                          else
                            select [ class "col-sm-9 form-control", name ("center-" ++ unpackFamilyId fam.familyId), onInput (CenterOptionChosen fam.familyId) ]
                                [ option [ value (unpackCenterId fam.centerId), selected True ] [ text (.name (findCenterByCenterId centers fam.centerId)) ]
                                , option [ value (unpackCenterId globalCenter.centerId) ] [ text globalCenter.name ]
                                ]
                        , button [ class "col-sm-2 col-md-2 btn btn-danger", onClick (RemoveFamilyFromSelected fam.familyId) ] [ text "Odebrat" ]
                        ]

                childRow : Child -> Html Msg
                childRow child =
                    div [ class "row" ]
                        [ span [ class "col-sm-1 col-md-2" ] [ text child.name ]
                        , span [ class "col-sm-1 col-md-1" ] [ text (String.fromInt child.age) ]
                        , span [ class "col-sm-4 col-md-9" ] [ text child.specifics ]
                        ]

                childrenRows : ChildList -> List (Html Msg)
                childrenRows children =
                    List.map childRow children
            in
            div [ class "margin-top-1-5em" ]
                (List.append
                    (childrenRows family.children)
                    [ familyCenterControls family ]
                )
    in
    div [] [ childRows ]


viewFamily : Family -> Html Msg
viewFamily family =
    let
        firstChild : Child
        firstChild =
            case List.head family.children of
                Just child ->
                    child

                Nothing ->
                    Child "" 0 NotImportant "" Nothing

        otherChildren : ChildList
        otherChildren =
            case List.tail family.children of
                Just children ->
                    children

                Nothing ->
                    []

        childRows =
            div [ class "row margin-top-1-5em family-background" ]
                [ viewSiblingsHeading family.children
                , span [ class "col-sm-2" ] [ text firstChild.name ]
                , span [ class "col-sm-1" ] [ text (String.fromInt firstChild.age) ]
                , span [ class "col-sm-5" ] [ text firstChild.specifics ]
                , childUrl firstChild
                , button [ class "col-sm-2 btn btn-primary", onClick (AddFamilyToSelected family.familyId) ] [ text "Vybrat" ]
                ]
                :: List.map
                    (\child ->
                        div [ class "row family-background" ]
                            [ span [ class "col-sm-2" ] [ text child.name ]
                            , span [ class "col-sm-1" ] [ text (String.fromInt child.age) ]
                            , span [ class "col-sm-5" ] [ text child.specifics ]
                            , childUrl child
                            , span [ class "col-sm-2" ] [ text "" ]
                            ]
                    )
                    otherChildren
    in
    div [] childRows


childUrl : Child -> Html Msg
childUrl child =
    case child.url of
        Just url ->
            a [ href url, target "_blank", class "col-sm-2" ] [ text "Odkaz" ]

        Nothing ->
            span [ class "col-sm-2" ] [ text "" ]


viewSiblingsHeading : ChildList -> Html Msg
viewSiblingsHeading children =
    if List.length children > 1 then
        span [ class "col-sm-12" ] [ b [] [ text "Sourozenci:" ] ]

    else
        span [] []


viewActivePlace : Model -> Html Msg
viewActivePlace model =
    let
        maybeChosenPlace =
            model.places
                |> List.filter (\place -> place.active == True)
                |> List.head
    in
    case maybeChosenPlace of
        Just place ->
            span [] [ text place.name ]

        Nothing ->
            span [] []
