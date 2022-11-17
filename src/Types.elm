module Types exposing
    ( Address
    , AppState(..)
    , Center
    , CenterId(..)
    , CenterList
    , Child
    , ChildList
    , Families
    , Family
    , FamilyId(..)
    , FamilyList
    , Gender(..)
    , KeydataApi
    , Model
    , Msg(..)
    , Place
    , PlaceId(..)
    , PlaceList
    , PostGiftApiResultType
    , PostGiftApiType
    , genderToString
    )

import Http as Http


type AppState
    = Start
    | CityChosen


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
    , globalUniversal : Bool
    , name : String
    , placeId : PlaceId
    , placeUniversal : Bool
    }


type alias CenterList =
    List Center


type alias Family =
    { centerId : CenterId
    , familyId : FamilyId
    , children : ChildList
    , placeId : PlaceId
    }


type alias FamilyList =
    List Family


type alias Families =
    { families : FamilyList
    }


type alias KeydataApi =
    { centers : CenterList
    , places : PlaceList
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
    { active : Bool
    , available : Bool
    , name : String
    , placeId : PlaceId
    }


type alias PlaceList =
    List Place


type Msg
    = AddFamilyToSelected FamilyId
    | CenterOptionChosen String
    | FetchFamilyResponse (Result Http.Error Families)
    | FetchKeydataResponse (Result Http.Error KeydataApi)
    | PlaceToggle PlaceId
    | PostDonorResponse (Result Http.Error PostGiftApiType)
    | RemoveFamilyFromSelected FamilyId
    | SendReservation
    | SetBottomThreshold Int
    | SetTopThreshold Int
    | SetGender Gender
    | ToggleAgreement
    | UpdateEmail String
    | UpdateEmail2 String
    | UpdateName String


type alias Model =
    { agreement : Bool
    , appState : AppState
    , bottomThreshold : Int
    , centers : CenterList
    , donorEmail : Maybe String
    , donorEmail2 : Maybe String
    , donorEmailErrorMessage : Maybe String
    , donorName : Maybe String
    , errorMessage : Maybe String
    , families : FamilyList
    , places : PlaceList
    , selectableCenters : CenterList
    , selectedCenterId : CenterId
    , selectedGender : Gender
    , selectedFamilies : FamilyList
    , successMessage : Maybe String
    , topThreshold : Int
    , viewableFamilies : FamilyList
    }
