module Center exposing
    ( Address
    , Center
    , CenterId(..)
    , CenterList
    , unpackCenterId
    )

import Place
    exposing
        ( PlaceId
        )


type alias Address =
    { city : String
    , street : String
    }


type CenterId
    = CenterId String


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


unpackCenterId : CenterId -> String
unpackCenterId centerId =
    case centerId of
        CenterId id ->
            id
