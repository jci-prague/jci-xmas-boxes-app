port module Main exposing (main)

import Browser as Browser
import Center
    exposing
        ( Center
        , CenterId(..)
        , CenterList
        )
import GlobalCenter
import Html exposing (Html, div)
import Http as Http
import Place exposing (PlaceId(..), PlaceList)
import Requests
    exposing
        ( fetchFamilies
        , fetchKeydata
        , postGift
        )
import Types
    exposing
        ( AppState(..)
        , Family
        , FamilyId
        , FamilyList
        , Gender(..)
        , Model
        , Msg(..)
        )
import Views
    exposing
        ( filterFormView
        , reservationFormView
        , viewFamilies
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CenterOptionChosen centerId ->
            ( { model | selectedCenterId = CenterId centerId }
            , Cmd.none
            )

        SetBottomThreshold threshold ->
            ( { model
                | bottomThreshold = threshold
                , viewableFamilies = filterViewableFamilies threshold model.topThreshold model.selectedGender model.places model.families model.selectedFamilies
              }
            , Cmd.none
            )

        SetTopThreshold threshold ->
            ( { model
                | topThreshold = threshold
                , viewableFamilies = filterViewableFamilies model.bottomThreshold threshold model.selectedGender model.places model.families model.selectedFamilies
              }
            , Cmd.none
            )

        SetGender gender ->
            ( { model
                | selectedGender = gender
                , viewableFamilies = filterViewableFamilies model.bottomThreshold model.topThreshold gender model.places model.families model.selectedFamilies
              }
            , Cmd.none
            )

        AddFamilyToSelected familyId ->
            let
                selectedFamilies : FamilyList
                selectedFamilies =
                    addToSelectedFamilies model familyId

                families : FamilyList
                families =
                    removeFromFamilies model familyId

                viewableFamilies : FamilyList
                viewableFamilies =
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender model.places families selectedFamilies

                chosenCenterId =
                    findSuitableCenterFromSelectedFamilies selectedFamilies model.selectableCenters
            in
            ( { model
                | families = families
                , selectedCenterId = chosenCenterId
                , selectedFamilies = selectedFamilies
                , viewableFamilies = viewableFamilies
              }
            , familyChosen ""
            )

        RemoveFamilyFromSelected familyId ->
            let
                selectedFamilies : FamilyList
                selectedFamilies =
                    removeFromSelectedFamilies model familyId

                families : FamilyList
                families =
                    addToFamilies model familyId

                viewableFamilies : FamilyList
                viewableFamilies =
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender model.places families selectedFamilies

                chosenCenterId =
                    findSuitableCenterFromSelectedFamilies selectedFamilies model.selectableCenters
            in
            ( { model
                | families = families
                , selectedCenterId = chosenCenterId
                , selectedFamilies = selectedFamilies
                , viewableFamilies = viewableFamilies
              }
            , Cmd.none
            )

        FetchFamilyResponse (Ok families) ->
            let
                viewableFamilies =
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender model.places families.families []
            in
            ( { model
                | families = families.families
                , selectedFamilies = []
                , viewableFamilies = viewableFamilies
              }
            , Cmd.none
            )

        FetchFamilyResponse (Err _) ->
            -- let
            --     x =
            --         Debug.log "Error: " e
            -- in
            ( model
            , Cmd.none
            )

        FetchKeydataResponse (Ok keydataApi) ->
            let
                inactivePlaces =
                    List.map (\place -> { place | active = False }) keydataApi.places
            in
            ( { model
                | centers = keydataApi.centers
                , places = inactivePlaces
                , selectableCenters = filterSelectableCenters inactivePlaces keydataApi.centers
                , selectedCenterId = (findGlobalUniversalCenter keydataApi.centers).centerId
              }
            , Cmd.none
            )

        FetchKeydataResponse (Err _) ->
            ( model
            , Cmd.none
            )

        UpdateName name ->
            ( { model | donorName = Just name }
            , Cmd.none
            )

        UpdateEmail email ->
            ( { model | donorEmail = Just email }
            , Cmd.none
            )

        UpdateEmail2 email ->
            let
                maybeDonorEmailErrorMessage : Maybe String
                maybeDonorEmailErrorMessage =
                    Maybe.andThen
                        (\donorEmail ->
                            if donorEmail /= email then
                                Just "Zadané emailové adresy se neshodují, prosím, zkontrolujte je a opravte."

                            else
                                Maybe.Nothing
                        )
                        model.donorEmail
            in
            ( { model | donorEmail2 = Just email, donorEmailErrorMessage = maybeDonorEmailErrorMessage }
            , Cmd.none
            )

        SendReservation ->
            ( { model
                | errorMessage = Nothing
                , successMessage = Nothing
              }
            , Cmd.batch [ postDonor model, donateClicked "" ]
            )

        PostDonorResponse (Ok _) ->
            ( { model
                | families = []
                , viewableFamilies = []
                , bottomThreshold = 1
                , topThreshold = 17
                , selectedGender = NotImportant
                , selectedFamilies = []
                , donorName = Maybe.Nothing
                , donorEmail = Maybe.Nothing
                , donorEmail2 = Maybe.Nothing
                , donorEmailErrorMessage = Maybe.Nothing
                , successMessage = Just "Vaše rezervace byla úspěšně zpracována. Na zadanou emailovou adresu Vám přijde potvrzovací email."
                , errorMessage = Maybe.Nothing
                , agreement = False
              }
            , fetchFamilies
            )

        PostDonorResponse (Err error) ->
            let
                errorString : String
                errorString =
                    case error of
                        Http.BadUrl url ->
                            "Bad URL: " ++ url

                        Http.Timeout ->
                            "Timeout"

                        Http.NetworkError ->
                            "Network Error"

                        Http.BadStatus response ->
                            "Bad Status: " ++ String.fromInt response

                        Http.BadBody message ->
                            "Bad Body: " ++ message
            in
            ( { model
                | errorMessage = Just ("Je nám líto, ale během zpracování Vaší rezervace nastala chyba. Zkuste to, prosím, ještě jednou. (" ++ errorString ++ ")")
              }
            , Cmd.none
            )

        PlaceToggle placeId ->
            let
                newPlaces =
                    List.map
                        (\place ->
                            if place.placeId == placeId then
                                { place | active = True }

                            else
                                { place | active = False }
                        )
                        model.places

                newViewableFamilies =
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender newPlaces model.families model.selectedFamilies

                newSelectableCenters =
                    filterSelectableCenters newPlaces model.centers
            in
            ( { model
                | appState = CityChosen
                , places = newPlaces
                , selectableCenters = newSelectableCenters
                , viewableFamilies = newViewableFamilies
              }
            , Cmd.none
            )

        ToggleAgreement ->
            ( { model | agreement = not model.agreement }
            , Cmd.none
            )


addToSelectedFamilies : Model -> FamilyId -> FamilyList
addToSelectedFamilies model familyId =
    findFamilyById model.families familyId :: model.selectedFamilies


removeFromSelectedFamilies : Model -> FamilyId -> FamilyList
removeFromSelectedFamilies model familyId =
    List.filter (\f -> familyId /= f.familyId) model.selectedFamilies


addToFamilies : Model -> FamilyId -> FamilyList
addToFamilies model familyId =
    findFamilyById model.selectedFamilies familyId :: model.families


removeFromFamilies : Model -> FamilyId -> FamilyList
removeFromFamilies model familyId =
    List.filter (\f -> familyId /= f.familyId) model.families


filterViewableFamilies : Int -> Int -> Gender -> PlaceList -> FamilyList -> FamilyList -> FamilyList
filterViewableFamilies bottom top gender places families selectedFamilies =
    List.filter
        (\f ->
            anyChildInAgeRangeAndGender bottom top gender f
                && not (List.any (\sf -> sf.familyId == f.familyId) selectedFamilies)
                && familyAtActivePlace places f
        )
        families


familyAtActivePlace : PlaceList -> Family -> Bool
familyAtActivePlace places family =
    let
        activePlaces =
            List.filter (\place -> place.active == True) places

        isFamilyAtActivePlace =
            List.any (\place -> place.placeId == family.placeId) activePlaces
    in
    isFamilyAtActivePlace


findFamilyById : FamilyList -> FamilyId -> Family
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
            { centerId = CenterId ""
            , familyId = familyId
            , children = []
            , placeId = PlaceId ""
            }


anyChildInAgeRangeAndGender : Int -> Int -> Gender -> Family -> Bool
anyChildInAgeRangeAndGender bottom top gender family =
    List.any
        (\child ->
            (child.age >= bottom && child.age <= top)
                && (gender == NotImportant || child.gender == gender)
        )
        family.children


filterSelectableCenters : PlaceList -> CenterList -> CenterList
filterSelectableCenters places centers =
    let
        activePlaces =
            List.filter (\place -> place.active) places

        selectableCenters =
            List.filter
                (\center ->
                    not center.globalUniversal
                        && List.any
                            (\activePlace ->
                                activePlace.placeId == center.placeId
                            )
                            activePlaces
                )
                centers

        globalUniversalCenter =
            findGlobalUniversalCenter centers
    in
    if List.length selectableCenters > 0 then
        selectableCenters

    else
        [ globalUniversalCenter ]


findSuitableCenterFromSelectedFamilies : FamilyList -> CenterList -> CenterId
findSuitableCenterFromSelectedFamilies selectedFamilies selectableCenters =
    let
        uniqueCentersFromSelectedFamilies =
            selectedFamilies
                |> List.map (\family -> family.centerId)
                |> listUnique

        chosenCenterId : CenterId
        chosenCenterId =
            if List.length uniqueCentersFromSelectedFamilies > 1 then
                let
                    maybeCenterToView =
                        selectableCenters
                            |> List.filter (\center -> center.placeUniversal == True)
                            |> List.head
                in
                case maybeCenterToView of
                    Just center ->
                        center.centerId

                    Nothing ->
                        failingUniversalCenter.centerId

            else
                let
                    maybeCenterIdToView =
                        uniqueCentersFromSelectedFamilies
                            |> List.head
                in
                Maybe.withDefault failingUniversalCenter.centerId maybeCenterIdToView
    in
    chosenCenterId


findGlobalUniversalCenter : CenterList -> Center
findGlobalUniversalCenter centers =
    centers
        |> List.filter (\center -> center.globalUniversal == True)
        |> List.head
        |> Maybe.withDefault failingUniversalCenter


failingUniversalCenter : Center
failingUniversalCenter =
    { address =
        { city = " vyber město, ve kterém budeš děti obdarovávat"
        , street = "Nejdříve, prosím"
        }
    , available = True
    , centerId = CenterId "00000"
    , globalUniversal = False
    , name = "Odběrové místo není k dispozici"
    , placeId = PlaceId "00000"
    , placeUniversal = False
    }


view : Model -> Html Msg
view model =
    if model.appState == Start then
        div []
            [ filterFormView model
            ]

    else
        div []
            [ filterFormView model
            , viewFamilies model
            , reservationFormView model
            ]


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { agreement = False
      , appState = Start
      , bottomThreshold = 1
      , centers = []
      , donorEmail = Maybe.Nothing
      , donorEmail2 = Maybe.Nothing
      , donorEmailErrorMessage = Maybe.Nothing
      , donorName = Maybe.Nothing
      , errorMessage = Maybe.Nothing
      , families = []
      , globalCenter = GlobalCenter.createMissingGlobalCenter
      , places = []
      , selectableCenters = []
      , selectedGender = NotImportant
      , selectedCenterId = CenterId "00000"
      , selectedFamilies = []
      , successMessage = Maybe.Nothing
      , topThreshold = 17
      , viewableFamilies = []
      }
    , Cmd.batch [ fetchKeydata, fetchFamilies ]
    )


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = initSubscriptions
        }


initSubscriptions : model -> Sub msg
initSubscriptions _ =
    Sub.none


postDonor : Model -> Cmd Msg
postDonor model =
    let
        name =
            case model.donorName of
                Just na ->
                    na

                Nothing ->
                    ""

        email =
            case model.donorEmail of
                Just em ->
                    em

                Nothing ->
                    ""

        families =
            model.selectedFamilies
    in
    if name /= "" && email /= "" then
        postGift families name email model.selectedCenterId

    else
        Cmd.none


listUnique : List a -> List a
listUnique inputList =
    -- https://medium.com/nerd-for-tech/how-to-get-unique-values-in-an-elm-list-d91ec7dfd0e
    let
        incUnique : a -> List a -> List a
        incUnique elem lst =
            if List.member elem lst then
                lst

            else
                elem :: lst
    in
    List.foldr incUnique [] inputList


port familyChosen : String -> Cmd msg


port donateClicked : String -> Cmd msg
