module PackageBrowser.Page.Home exposing (..)

import Database.Package as Package
import Database.Package.Decode
import Element
import Element.Keyed
import Element.Lazy as Lazy
import Elm.Module
import Elm.Package
import Http
import Json.Decode as Decode
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)


type alias Context a b =
    { a
        | router :
            { b
                | view : Router.View
            }
    }



--


type alias Model =
    { search : String
    , packages : Result PackagesError (List Package.Package)
    }


type PackagesError
    = LoadingPackages
    | PackagesHttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { search = ""
      , packages = Err LoadingPackages
      }
    , getPackages
    )


getPackages : Cmd Msg
getPackages =
    Http.get
        { url = "db/packages.json"
        , expect = Http.expectJson GotPackages (Decode.list Database.Package.Decode.package)
        }



--


type Msg
    = SearchChanged String
    | GotPackages (Result Http.Error (List Package.Package))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SearchChanged a ->
            ( { model | search = a }
            , Cmd.none
            )

        GotPackages a ->
            ( { model | packages = a |> Result.mapError PackagesHttpError }
            , Cmd.none
            )



--


view : Context a b -> Model -> Element Msg
view ctx model =
    row
        [ Element.height Element.fill
        ]
        [ Lazy.lazy viewFirst model
        , Lazy.lazy viewSecond ctx.router.view
        ]


