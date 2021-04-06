module PackageBrowser.Strings exposing (..)

import Http


title =
    "Elm Packages Browser"


info =
    "Info"


infoText1 =
    "Packages compatible with Elm 0.19.1."


infoText2 =
    "Showing package latest version from 1 Apr 2021."


infoText3 =
    "Use prefix search. For \"mdgriffith/elm-ui\" type \"e ui md\"."


proposalLink =
    "Official proposal"


ok =
    "OK"


ellipsis =
    "…"


searchInput =
    "Search in package and module names"


loading =
    "Loading..."


noPackagesFound =
    "No packages found."


packageNotFound =
    "Package not found."


moduleNotFound =
    "Module not found."


readmeIsNotAvailable =
    "Readme is not available."


source =
    "Source"


officialDocs =
    "Docs"


index =
    "Index"


httpError a =
    case a of
        Http.BadUrl _ ->
            "Failed to connect to server."

        Http.Timeout ->
            "Failed to connect to server."

        Http.NetworkError ->
            "Failed to connect to server."

        Http.BadStatus _ ->
            "Server seems to be broken."

        Http.BadBody _ ->
            "Server seems to be broken."
