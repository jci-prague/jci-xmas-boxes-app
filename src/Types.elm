module Types exposing
    ( Child
    , ChildList
    , Families
    , Family
    , FamilyId(..)
    , FamilyList
    , Gender(..)
    , Model
    , Msg(..)
    , PostGiftApiResultType
    , PostGiftApiType
    , genderToString
    )

import Http exposing (Error)


type Gender
    = Male
    | Female
    | NotImportant


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"

        NotImportant ->
            "Not Important"


type FamilyId
    = FamilyId String


type alias Child =
    { name : String
    , age : Int
    , gender : Gender
    }


type alias ChildList =
    List Child


type alias Family =
    { familyId : FamilyId
    , children : ChildList
    }


type alias FamilyList =
    List Family


type alias Families =
    { families : FamilyList
    }


type alias PostGiftApiResultType =
    { familyId : FamilyId
    , success : Bool
    , errors : List String
    }


type alias PostGiftApiType =
    { success : Bool
    , results : List PostGiftApiResultType
    }


type Msg
    = SetBottomThreshold Int
    | SetTopThreshold Int
    | SetGender Gender
    | AddFamilyToSelected FamilyId
    | RemoveFamilyFromSelected FamilyId
    | SendReservation
    | FetchFamilyResponse (Result Http.Error Families)
    | UpdateName String
    | UpdateEmail String
    | PostDonorResponse (Result Http.Error PostGiftApiType)
    | None


type alias Model =
    { families : FamilyList
    , viewableFamilies : FamilyList
    , bottomThreshold : Int
    , topThreshold : Int
    , selectedGender : Gender
    , selectedFamilies : FamilyList
    , donorName : Maybe String
    , donorEmail : Maybe String
    , successMessage: Maybe String
    , errorMessage: Maybe String
    }
