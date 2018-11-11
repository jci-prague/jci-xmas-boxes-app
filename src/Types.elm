module Types exposing (Child, ChildList, Family, FamilyId(..), Gender(..), Msg(..))


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


type Msg
    = SetBottomThreshold Int
    | SetTopThreshold Int
    | SetGender Gender
    | AddFamilyToSelected FamilyId
    | RemoveFamilyFromSelected FamilyId
    | SendReservation
    | None
