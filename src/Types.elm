module Types exposing (Child, ChildList, Families, Family, FamilyId(..), FamilyList, Gender(..), Msg(..))

import Http exposing (Error)


type Gender
    = Male
    | Female
    | NotImportant


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
    | None
