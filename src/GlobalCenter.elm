module GlobalCenter exposing
    ( GlobalCenter
    , createDefinedGlobalCenter
    , createMissingGlobalCenter
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


isGlobalCenterDefined : GlobalCenter -> Bool
isGlobalCenterDefined gc =
    case gc of
        GlobalCenterDefined _ ->
            True

        GlobalCenterMissing ->
            False
