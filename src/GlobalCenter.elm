module GlobalCenter exposing
    ( GlobalCenter(..)
    , createDefinedGlobalCenter
    , createMissingGlobalCenter
    , extractCenter
    , isGlobalCenterDefined
    )

import Center exposing (Center)


type GlobalCenter
    = GlobalCenterMissing
    | GlobalCenterDefined Center


createMissingGlobalCenter : GlobalCenter
createMissingGlobalCenter =
    GlobalCenterMissing


createDefinedGlobalCenter : Center -> GlobalCenter
createDefinedGlobalCenter center =
    GlobalCenterDefined center


extractCenter : GlobalCenter -> Maybe Center
extractCenter gc =
    case gc of
        GlobalCenterDefined center ->
            Just center

        GlobalCenterMissing ->
            Nothing


isGlobalCenterDefined : GlobalCenter -> Bool
isGlobalCenterDefined gc =
    case gc of
        GlobalCenterDefined _ ->
            True

        GlobalCenterMissing ->
            False
