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
        , id
        , name
        , placeholder
        , selected
        , style
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
                [ div [ class "col-3" ] [ text "Vybrané město" ]
                , div [ class "col-3" ] [ viewActivePlace model ]
                , div [ class "col-6" ]
                    [ a [ onClick ResetForm, style "cursor" "pointer" ] [ text "Změnit město" ] ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col-3" ] [ text "Věk od" ]
                , span [ class "col-9" ]
                    [ select [ id "bottom-threshold", onInput SetBottomThreshold ]
                        [ option [ value "1", selected True ] [ text "1" ]
                        , option [ value "2" ] [ text "2" ]
                        , option [ value "3" ] [ text "3" ]
                        , option [ value "4" ] [ text "4" ]
                        , option [ value "5" ] [ text "5" ]
                        , option [ value "6" ] [ text "6" ]
                        , option [ value "7" ] [ text "7" ]
                        , option [ value "8" ] [ text "8" ]
                        , option [ value "9" ] [ text "9" ]
                        , option [ value "10" ] [ text "10" ]
                        , option [ value "11" ] [ text "11" ]
                        , option [ value "12" ] [ text "12" ]
                        , option [ value "13" ] [ text "13" ]
                        , option [ value "14" ] [ text "14" ]
                        , option [ value "15" ] [ text "15" ]
                        , option [ value "16" ] [ text "16" ]
                        , option [ value "17" ] [ text "17" ]
                        , option [ value "18" ] [ text "18" ]
                        , option [ value "19" ] [ text "19" ]
                        ]
                    ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col-3" ] [ text "Věk do" ]
                , span [ class "col-9" ]
                    [ select [ id "top-threshold", onInput SetTopThreshold ]
                        [ option [ value "2" ] [ text "2" ]
                        , option [ value "3" ] [ text "3" ]
                        , option [ value "4" ] [ text "4" ]
                        , option [ value "5" ] [ text "5" ]
                        , option [ value "6" ] [ text "6" ]
                        , option [ value "7" ] [ text "7" ]
                        , option [ value "8" ] [ text "8" ]
                        , option [ value "9" ] [ text "9" ]
                        , option [ value "10" ] [ text "10" ]
                        , option [ value "11" ] [ text "11" ]
                        , option [ value "12" ] [ text "12" ]
                        , option [ value "13" ] [ text "13" ]
                        , option [ value "14" ] [ text "14" ]
                        , option [ value "15" ] [ text "15" ]
                        , option [ value "16" ] [ text "16" ]
                        , option [ value "17" ] [ text "17" ]
                        , option [ value "18" ] [ text "18" ]
                        , option [ value "19" ] [ text "19" ]
                        , option [ value "20", selected True ] [ text "20" ]
                        ]
                    ]
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
                [ div [ class "col" ]
                    [ text "Můžeš obdarovat jedno dítě, sourozence (skupina dětí se společným tlačítkem "
                    , b [] [ text "'VYBRAT'" ]
                    , text " nebo i více dětí najednou. Tvoje vybrané děti se objevují ve formuláři pod seznamem. Po kliknutí na tlačítko "
                    , b [] [ text "'VYBRAT'" ]
                    , text ", odskroluj až úplně dolů a dokonči proces vyplněním formuláře a klinutím na tlačítko "
                    , b [] [ text "'OBDAROVAT'" ]
                    , text ". Po úspěšné odeslání Ti bude obratem zaslán e-mail se všemi informacemi."
                    ]
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
                        button [ class "col btn", onClick (PlaceToggle place.placeId) ]
                            [ text place.name ]
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
            , if model.distinctSelectedCentersAmount > 1 then
                div [ class "row" ]
                    [ span [ class "col" ] []
                    , span [ class "col-9 alert alert-primary" ] [ text "Upozorňujeme, že máte vybrány děti/sourozence pro více jak jednoho sběrné místo. Není to žádný problém, jen, prosím, zaneste krabice do příslušných sběrných míst, velmi děkujeme! Tým Vanočních krabic" ]
                    ]

              else
                span [] []
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
