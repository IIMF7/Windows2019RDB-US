module PackageBrowser.Strings exposing (..)

import Http


title =
    "Pravdomil's Elm Packages Browser"


info =
    "Info"


infoText1 =
    "Data is from 27 Mar 2021."


infoText2 =
    "Packages compatible with Elm 0.19.1."


infoText3 =
    "Showing only package latest version."


infoText4 =
    "Use prefix search, for \"mdgriffith/elm-ui\" type \"e ui md\"."


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


source =
    "Source"


officialDocs =
    "Official Docs"


httpError a =
    case a of
        Http.BadUrl _ ->
            "Connection failed."

        Http.Timeout ->
            "Connection failed."

        Http.NetworkError ->
            "Connection failed."

        Http.BadStatus _ ->
            "Server seems to be broken."

        Http.BadBody _ ->
            "Server seems to be broken."
