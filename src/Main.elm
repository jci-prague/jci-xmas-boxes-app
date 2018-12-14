module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (get, send)
import Json.Decode as Decode exposing (string)
import List exposing (..)
import Requests exposing (..)
import Types exposing (..)


type alias Model =
    { families : FamilyList
    , viewableFamilies : FamilyList
    , bottomThreshold : Int
    , topThreshold : Int
    , selectedGender : Gender
    , selectedFamilies : FamilyList
    , donorName : Maybe String
    , donorEmail : Maybe String
    }


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
                [ span [ class "col" ] [ text "" ]
                , div [ class "col-9" ] [ viewSelectedFamilies model ]
                ]
            , div [ class "row form-group" ]
                [ span [ class "col" ] [ text "" ]
                , button [ class "col-9 btn btn-primary", onClick SendReservation ] [ text "Zaregistrovat se" ]
                ]
            ]
        ]


filterFormView : Model -> Html Msg
filterFormView model =
    div []
        [ div []
            [ span [] [ text "Věk: " ]
            , span []
                [ text (String.fromInt model.bottomThreshold ++ " - " ++ String.fromInt model.topThreshold)
                ]
            , span []
                [ text ("(" ++ genderToString model.selectedGender ++ ")")
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
    div [ class "row" ]
        [ div [ class "col" ]
            (List.map
                (\f ->
                    viewSelectedFamily f
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
            (\child ->
                div [ class "row margin-bottom-1em" ]
                    [ span [ class "col" ] [ text child.name ]
                    , span [ class "col" ] [ text (String.fromInt child.age) ]
                    , button [ class "col btn btn-danger", onClick (RemoveFamilyFromSelected family.familyId) ] [ text "Odebrat" ]
                    ]
            )
            family.children
        )


viewFamily : Family -> Html Msg
viewFamily family =
    div []
        (List.map
            (\child ->
                div [ class (classForGender child) ]
                    [ span [ class "childName" ] [ text child.name ]
                    , span [ class "childAge" ] [ text (String.fromInt child.age) ]
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
