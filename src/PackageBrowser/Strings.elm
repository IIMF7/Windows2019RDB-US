module PackageBrowser.Strings exposing (..)

import Http


title =
    "Pravdomil's Elm Packages Browser"


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
