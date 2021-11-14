module Types exposing
    ( Address
    , Center
    , CenterId(..)
    , Child
    , ChildList
    , Families
    , Family
    , FamilyId(..)
    , FamilyList
    , Gender(..)
    , Model
    , Msg(..)
    , Place
    , PlaceId(..)
    , PostGiftApiResultType
    , PostGiftApiType
    , genderToString
    )

import Http as Http


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


type CenterId
    = CenterId String


type FamilyId
    = FamilyId String


type PlaceId
    = PlaceId String


type alias Address =
    { city : String
    , street : String
    }


type alias Child =
    { name : String
    , age : Int
    , gender : Gender
    , specifics : String
    }


type alias ChildList =
    List Child


type alias Center =
    { address : Address
    , available : Bool
    , centerId : CenterId
    , name : String
    , placeId : PlaceId
    , universal : Bool
    }


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


type alias Place =
    { available : Bool
    , name : String
    , placeId : PlaceId
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
    | ToggleAgreement


type alias Model =
    { families : FamilyList
    , viewableFamilies : FamilyList
    , bottomThreshold : Int
    , topThreshold : Int
    , selectedGender : Gender
    , selectedFamilies : FamilyList
    , donorName : Maybe String
    , donorEmail : Maybe String
    , successMessage : Maybe String
    , errorMessage : Maybe String
    , agreement : Bool
    }
