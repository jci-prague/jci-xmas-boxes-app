module Place exposing (Place, PlaceId(..), PlaceList)


type PlaceId
    = PlaceId String


type alias Place =
    { active : Bool
    , available : Bool
    , name : String
    , placeId : PlaceId
    }


type alias PlaceList =
    List Place
