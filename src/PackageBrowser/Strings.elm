module PackageBrowser.Strings exposing (..)

import Http


title =
    "Pravdomil's Elm Packages Browser"


searchInput =
    "Search package name or module name"


loading =
    "Loading..."


noPackagesFound =
    "No packages found."


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
