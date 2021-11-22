port module Main exposing (main)

import Browser as Browser exposing (element)
import Html exposing (Html, div)
import Http as Http
import Requests
    exposing
        ( fetchFamilies
        , fetchKeydata
        , postGift
        )
import Types
    exposing
        ( CenterId(..)
        , CenterList
        , Families
        , Family
        , FamilyId
        , FamilyList
        , Gender(..)
        , Model
        , Msg(..)
        , PlaceId(..)
        , PlaceList
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
            ( { model | selectedCenterId = Just (CenterId centerId) }
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
            in
            ( { model
                | selectedFamilies = selectedFamilies
                , families = families
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
            in
            ( { model
                | selectedFamilies = selectedFamilies
                , families = families
                , viewableFamilies = viewableFamilies
              }
            , Cmd.none
            )

        FetchFamilyResponse (Ok families) ->
            ( { model
                | families = families.families
                , selectedFamilies = []
                , viewableFamilies = families.families
              }
            , Cmd.none
            )

        FetchFamilyResponse (Err _) ->
            ( model
            , Cmd.none
            )

        FetchKeydataResponse (Ok keydataApi) ->
            ( { model
                | centers = keydataApi.centers
                , places = keydataApi.places
                , selectableCenters = filterSelectableCenters keydataApi.places keydataApi.centers
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
                                { place | active = not place.active }

                            else
                                place
                        )
                        model.places

                newViewableFamilies =
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender newPlaces model.families model.selectedFamilies

                newSelectableCenters =
                    filterSelectableCenters newPlaces model.centers
            in
            ( { model
                | places = newPlaces
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
                    not center.universal
                        && List.any
                            (\activePlace ->
                                activePlace.placeId == center.placeId
                            )
                            activePlaces
                )
                centers

        universalCenters =
            List.filter (\center -> center.universal == True) centers
    in
    List.concat [ selectableCenters, universalCenters ]


view : Model -> Html Msg
view model =
    div []
        [ filterFormView model
        , viewFamilies model
        , reservationFormView model
        ]


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { agreement = False
      , bottomThreshold = 1
      , centers = []
      , donorEmail = Maybe.Nothing
      , donorName = Maybe.Nothing
      , errorMessage = Maybe.Nothing
      , families = []
      , places = []
      , selectableCenters = []
      , selectedGender = NotImportant
      , selectedCenterId = Maybe.Nothing
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
        postGift families name email

    else
        Cmd.none


port familyChosen : String -> Cmd msg


port donateClicked : String -> Cmd msg
