module Requests exposing
    ( fetchFamilies
    , fetchKeydata
    , postGift
    )

import Center
    exposing
        ( Address
        , Center
        , CenterId(..)
        , CenterList
        )
import Http as Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , int
        , string
        )
import Json.Decode.Pipeline
    exposing
        ( hardcoded
        , required
        )
import Json.Encode as Encode exposing (Value)
import Place
    exposing
        ( Place
        , PlaceId(..)
        , PlaceList
        )
import Types
    exposing
        ( Child
        , ChildList
        , Families
        , Family
        , FamilyId(..)
        , FamilyList
        , Gender(..)
        , KeydataApi
        , Msg(..)
        , PostGiftApiResultType
        , PostGiftApiType
        )


fetchFamilies : Cmd Msg
fetchFamilies =
    let
        req =
            { url = "/api/family"
            , expect = Http.expectJson FetchFamilyResponse familiesDecoder
            }
    in
    Http.get req


fetchKeydata : Cmd Msg
fetchKeydata =
    let
        req =
            { url = "/api/keydata"
            , expect = Http.expectJson FetchKeydataResponse keydataApiDecoder
            }
    in
    Http.get req


postGift : FamilyList -> String -> String -> CenterId -> Cmd Msg
postGift familyList name email centerId =
    let
        req =
            { body = Http.jsonBody (giftEncoder familyList name email centerId)
            , url = "/api/family/gift"
            , expect = Http.expectJson PostDonorResponse postGiftApiTypeDecoder
            }
    in
    Http.post req


addressDecoder : Decoder Address
addressDecoder =
    Decode.succeed Address
        |> required "city" string
        |> required "street" string


centerDecoder : Decoder Center
centerDecoder =
    Decode.succeed Center
        |> required "address" addressDecoder
        |> required "available" bool
        |> required "id" centerIdDecoder
        |> required "globalUniversal" bool
        |> required "name" string
        |> required "place" placeIdDecoder
        |> required "placeUniversal" bool


centerListDecoder : Decoder CenterList
centerListDecoder =
    Decode.list centerDecoder


centerIdDecoder : Decoder CenterId
centerIdDecoder =
    Decode.string
        |> Decode.andThen
            (\id ->
                Decode.succeed (CenterId id)
            )


giftEncoder : FamilyList -> String -> String -> CenterId -> Value
giftEncoder families name email centerId =
    let
        familyIds : List FamilyId
        familyIds =
            List.map (\f -> f.familyId) families

        centerIdString =
            case centerId of
                CenterId id ->
                    id
    in
    Encode.object
        [ ( "centerId", Encode.string centerIdString )
        , ( "donor", donorEncoder name email )
        , ( "familyIds", familyIdListEncoder familyIds )
        ]


donorEncoder : String -> String -> Value
donorEncoder name email =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "email", Encode.string email )
        ]


familyIdEncoder : FamilyId -> Value
familyIdEncoder (FamilyId id) =
    Encode.string id


familyIdListEncoder : List FamilyId -> Value
familyIdListEncoder familyIds =
    Encode.list familyIdEncoder familyIds


genderDecoder : Decoder Gender
genderDecoder =
    Decode.string
        |> Decode.andThen
            (\g ->
                case g of
                    "Male" ->
                        Decode.succeed Male

                    "Female" ->
                        Decode.succeed Female

                    unknownGender ->
                        Decode.fail <| "Unknown gender: " ++ unknownGender
            )


childDecoder : Decoder Child
childDecoder =
    Decode.succeed Child
        |> required "name" string
        |> required "age" int
        |> required "gender" genderDecoder
        |> required "specifics" string


childListDecoder : Decoder ChildList
childListDecoder =
    Decode.list childDecoder


familyIdDecoder : Decoder FamilyId
familyIdDecoder =
    Decode.string
        |> Decode.andThen
            (\id ->
                Decode.succeed (FamilyId id)
            )


familyDecoder : Decoder Family
familyDecoder =
    Decode.succeed Family
        |> required "centerId" centerIdDecoder
        |> required "id" familyIdDecoder
        |> required "children" childListDecoder
        |> required "placeId" placeIdDecoder


familyListDecoder : Decoder FamilyList
familyListDecoder =
    Decode.list familyDecoder


familiesDecoder : Decoder Families
familiesDecoder =
    Decode.succeed Families
        |> required "families" familyListDecoder


keydataApiDecoder : Decoder KeydataApi
keydataApiDecoder =
    Decode.succeed KeydataApi
        |> required "centers" centerListDecoder
        |> required "places" placeListDecoder


placeDecoder : Decoder Place
placeDecoder =
    Decode.succeed Place
        |> hardcoded True
        |> required "available" bool
        |> required "name" string
        |> required "id" placeIdDecoder


placeIdDecoder : Decoder PlaceId
placeIdDecoder =
    Decode.string
        |> Decode.andThen
            (\id ->
                Decode.succeed (PlaceId id)
            )


placeListDecoder : Decoder PlaceList
placeListDecoder =
    Decode.list placeDecoder


postGiftApiResultTypeDecoder : Decoder PostGiftApiResultType
postGiftApiResultTypeDecoder =
    Decode.succeed PostGiftApiResultType
        |> required "familyId" familyIdDecoder
        |> required "success" bool
        |> required "errors" (Decode.list string)


postGiftApiTypeDecoder : Decoder PostGiftApiType
postGiftApiTypeDecoder =
    Decode.succeed PostGiftApiType
        |> required "success" bool
        |> required "results" (Decode.list postGiftApiResultTypeDecoder)
