module Types exposing
    ( AppState(..)
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
    , PostGiftApiResultType
    , PostGiftApiType
    , genderToString
    )

import Center
    exposing
        ( CenterId(..)
        , CenterList
        )
import GlobalCenter
    exposing
        ( GlobalCenter
        )
import Http as Http
import Place
    exposing
        ( PlaceId
        , PlaceList
        )


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


type FamilyId
    = FamilyId String


type alias Child =
    { name : String
    , age : Int
    , gender : Gender
    , specifics : String
    }


type alias ChildList =
    List Child


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
    , globalCenter : GlobalCenter
    , places : PlaceList
    , selectableCenters : CenterList
    , selectedCenterId : Maybe CenterId
    , selectedGender : Gender
    , selectedFamilies : FamilyList
    , successMessage : Maybe String
    , topThreshold : Int
    , viewableFamilies : FamilyList
    }
