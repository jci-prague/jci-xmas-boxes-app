module Center exposing
    ( Address
    , Center
    , CenterId(..)
    , CenterList
    , failingUniversalCenter
    , unpackCenterId
    )

import Place
    exposing
        ( PlaceId(..)
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


failingUniversalCenter : Center
failingUniversalCenter =
    { address =
        { city = " vyber město, ve kterém budeš děti obdarovávat"
        , street = "Nejdříve, prosím"
        }
    , available = True
    , centerId = CenterId "00000"
    , globalUniversal = False
    , name = "Odběrové místo není k dispozici"
    , placeId = PlaceId "00000"
    , placeUniversal = False
    }


unpackCenterId : CenterId -> String
unpackCenterId centerId =
    case centerId of
        CenterId id ->
            id
