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
                , viewableFamilies = updateViewableFamilies threshold model.topThreshold model.selectedGender model.families
              }
            , Cmd.none
            )

        SetTopThreshold threshold ->
            ( { model
                | topThreshold = threshold
                , viewableFamilies = updateViewableFamilies model.bottomThreshold threshold model.selectedGender model.families
              }
            , Cmd.none
            )

        SetGender gender ->
            ( { model
                | selectedGender = gender
                , viewableFamilies = updateViewableFamilies model.bottomThreshold model.topThreshold gender model.families
              }
            , Cmd.none
            )

        AddFamilyToSelected familyId ->
            ( { model
                | selectedFamilies = addToSelectedFamilies model familyId
                , viewableFamilies = removeFromViewableFamilies model familyId
              }
            , Cmd.none
            )

        RemoveFamilyFromSelected familyId ->
            ( { model
                | selectedFamilies = removeFromSelectedFamilies model familyId
                , viewableFamilies = addToViewableFamilies model familyId
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


addToSelectedFamilies : Model -> FamilyId -> List Family
addToSelectedFamilies model familyId =
    findFamilyById model.viewableFamilies familyId :: model.selectedFamilies


addToViewableFamilies : Model -> FamilyId -> List Family
addToViewableFamilies model familyId =
    findFamilyById model.selectedFamilies familyId :: model.viewableFamilies


removeFromSelectedFamilies : Model -> FamilyId -> List Family
removeFromSelectedFamilies model familyId =
    List.filter (\f -> familyId /= f.familyId) model.selectedFamilies


removeFromViewableFamilies : Model -> FamilyId -> List Family
removeFromViewableFamilies model familyId =
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
