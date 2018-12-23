module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, div)
import Http exposing (get, send)
import Json.Decode as Decode exposing (string)
import List exposing (..)
import Requests exposing (..)
import Types exposing (..)
import Views exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBottomThreshold threshold ->
            ( { model
                | bottomThreshold = threshold
                , viewableFamilies = filterViewableFamilies threshold model.topThreshold model.selectedGender model.families model.selectedFamilies
              }
            , Cmd.none
            )

        SetTopThreshold threshold ->
            ( { model
                | topThreshold = threshold
                , viewableFamilies = filterViewableFamilies model.bottomThreshold threshold model.selectedGender model.families model.selectedFamilies
              }
            , Cmd.none
            )

        SetGender gender ->
            ( { model
                | selectedGender = gender
                , viewableFamilies = filterViewableFamilies model.bottomThreshold model.topThreshold gender model.families model.selectedFamilies
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
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender families selectedFamilies
            in
            ( { model
                | selectedFamilies = selectedFamilies
                , families = families
                , viewableFamilies = viewableFamilies
              }
            , Cmd.none
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
                    filterViewableFamilies model.bottomThreshold model.topThreshold model.selectedGender families selectedFamilies
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
                , viewableFamilies = families.families
              }
            , Cmd.none
            )

        FetchFamilyResponse (Err _) ->
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
            ( model
            , postDonor model
            )

        PostDonorResponse (Ok resultString) ->
            ( { model
                | donorName = Maybe.Nothing
                , donorEmail = Maybe.Nothing
              }
            , Cmd.none
            )

        PostDonorResponse (Err _) ->
            ( model
            , Cmd.none
            )

        None ->
            ( model
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


filterViewableFamilies : Int -> Int -> Gender -> FamilyList -> FamilyList -> FamilyList
filterViewableFamilies bottom top gender families selectedFamilies =
    List.filter
        (\f ->
            anyChildInAgeRangeAndGender bottom top gender f
                && not (List.any (\sf -> sf.familyId == f.familyId) selectedFamilies)
        )
        families


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
            { familyId = familyId, children = [] }


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
        , viewFamilies model
        , reservationFormView model
        ]


initialModel : () -> ( Model, Cmd Msg )
initialModel aString =
    ( { families = []
      , viewableFamilies = []
      , bottomThreshold = 1
      , topThreshold = 17
      , selectedGender = NotImportant
      , selectedFamilies = []
      , donorName = Maybe.Nothing
      , donorEmail = Maybe.Nothing
      }
    , fetchFamilyList
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
initSubscriptions model =
    Sub.none


fetchFamilyList : Cmd Msg
fetchFamilyList =
    Http.send FetchFamilyResponse getFamilyList


getFamilyList : Http.Request Families
getFamilyList =
    Http.get "/api/family" familiesDecoder


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
        Http.post "/api/family/gift" (Http.jsonBody (Requests.giftEncoder families name email)) string
            |> Http.send PostDonorResponse

    else
        Cmd.none
