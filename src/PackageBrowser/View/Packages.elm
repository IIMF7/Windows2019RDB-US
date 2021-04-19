module PackageBrowser.View.Packages exposing (..)

import Browser.Dom
import Database.Package as Package
import Database.Package.Decode
import Element
import Element.Keyed
import Element.Lazy as Lazy
import Element.Virtualized
import Elm.Module
import Elm.Package
import Elm.Package.NameDict as NameDict
import Http
import Json.Decode as Decode
import PackageBrowser.Router as Router
import PackageBrowser.Translation as Translation
import PackageBrowser.Ui exposing (..)
import PackageBrowser.Ui.Status as Status
import Regex
import Task


type alias Context a b c =
    { a
        | router :
            { b
                | view : Router.View
            }
        , header :
            { c
                | search : String
            }
    }



--


type alias Model =
    { packages : Result Error (List Package.Package)
    , scrollOffset : Float
    , expanded : NameDict.NameDict ()
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { packages = Err Loading
      , scrollOffset = 0
      , expanded = initialExpanded
      }
    , getPackages
    )


initialExpanded : NameDict.NameDict ()
initialExpanded =
    [ Elm.Package.fromString "elm/core" ]
        |> List.filterMap identity
        |> List.map (\v -> ( v, () ))
        |> NameDict.fromList


getPackages : Cmd Msg
getPackages =
    Http.get
        { url = "db/packages.json"
        , expect = Http.expectJson GotPackages (Decode.list Database.Package.Decode.package)
        }



--


type Msg
    = UrlChanged
    | GotPackages (Result Http.Error (List Package.Package))
    | Reveal Elm.Package.Name
    | ViewportSet (Result Browser.Dom.Error ())
    | SearchChanged
    | ScrollOffsetChanged Float
    | RestoreScrollOffset


update : Context a b c -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        UrlChanged ->
            case ctx.router.view of
                Router.DefaultView ->
                    ( { model | expanded = initialExpanded }
                    , scrollToTop ctx model
                    )

                _ ->
                    let
                        expanded : NameDict.NameDict ()
                        expanded =
                            case ctx.router.view |> Router.viewToPackageName of
                                Just b ->
                                    model.expanded |> NameDict.insert b ()

                                Nothing ->
                                    model.expanded
                    in
                    ( { model | expanded = expanded }
                    , Cmd.none
                    )

        GotPackages a ->
            let
                package : Maybe Elm.Package.Name
                package =
                    case ctx.router.view |> Router.viewToPackageName of
                        Just b ->
                            Just b

                        Nothing ->
                            Elm.Package.fromString "elm/core"

                nextModel : Model
                nextModel =
                    { model
                        | packages = a |> Result.mapError HttpError
                    }
            in
            ( nextModel
            , package |> Maybe.andThen (scrollToPackage ctx nextModel) |> Maybe.withDefault Cmd.none
            )

        Reveal a ->
            ( model
            , a |> scrollToPackage ctx model |> Maybe.withDefault Cmd.none
            )

        ViewportSet _ ->
            ( model
            , Cmd.none
            )

        SearchChanged ->
            ( model
            , scrollToTop ctx model
            )

        ScrollOffsetChanged a ->
            ( { model | scrollOffset = a }
            , Cmd.none
            )

        RestoreScrollOffset ->
            ( model
            , Browser.Dom.setViewportOf packagesId 0 model.scrollOffset
                |> Task.attempt ViewportSet
            )


scrollToTop : Context a b c -> Model -> Cmd Msg
scrollToTop ctx model =
    if ctx.header.search |> String.trim |> String.isEmpty then
        Elm.Package.fromString "elm/core"
            |> Maybe.andThen (scrollToPackage ctx model)
            |> Maybe.withDefault Cmd.none

    else
        Browser.Dom.setViewportOf packagesId 0 0
            |> Task.attempt ViewportSet


scrollToPackage : Context a b c -> Model -> Elm.Package.Name -> Maybe (Cmd Msg)
scrollToPackage ctx model a =
    model.packages
        |> Result.toMaybe
        |> Maybe.andThen
            (\v ->
                Element.Virtualized.getScrollOffset
                    { data = v |> filterPackages ctx.header.search
                    , getKey = .name >> Elm.Package.toString
                    , getSize = \vv -> computeSize (NameDict.member vv.name model.expanded) vv
                    , key = Elm.Package.toString a
                    }
            )
        |> Maybe.map
            (\v ->
                Browser.Dom.setViewportOf packagesId 0 (toFloat v)
                    |> Task.attempt ViewportSet
            )



--


modulesLimit : Int
modulesLimit =
    6


view : String -> Router.View -> Model -> Element Msg
view search view_ model =
    case model.packages of
        Ok b ->
            case filterPackages search b of
                [] ->
                    Status.view []
                        [ text Translation.noPackagesFound
                        ]

                c ->
                    Element.Virtualized.column [ paddingXY 0 4, id packagesId ]
                        { data = c
                        , getKey = .name >> Elm.Package.toString
                        , getSize = \v -> computeSize (NameDict.member v.name model.expanded) v
                        , scrollOffset = model.scrollOffset
                        , view =
                            \v ->
                                Lazy.lazy3 viewPackage
                                    (NameDict.member v.name model.expanded)
                                    (activePackageAndModule view_ v)
                                    v
                        , onScroll = ScrollOffsetChanged
                        }

        Err b ->
            Status.view []
                [ case b of
                    Loading ->
                        text Translation.loading

                    HttpError c ->
                        text (Translation.httpError c)
                ]


computeSize : Bool -> Package.Package -> Int
computeSize expand a =
    let
        modulesCount : Int
        modulesCount =
            a.exposed |> Package.exposedToList |> List.length

        count =
            if not expand && modulesCount > modulesLimit then
                modulesLimit

            else
                modulesCount
    in
    24 + count * 20 + 12


viewPackage : Bool -> Maybe (Maybe Elm.Module.Name) -> Package.Package -> Element msg
viewPackage expand active a =
    let
        ( shortened, modules ) =
            let
                modules_ : List Elm.Module.Name
                modules_ =
                    a.exposed |> Package.exposedToList
            in
            if expand == False && List.length modules_ > modulesLimit then
                ( True, modules_ |> List.take (modulesLimit - 1) )

            else
                ( False, modules_ )

        packageColor : Element.Attribute msg
        packageColor =
            if active == Just Nothing then
                noneAttribute

            else
                fontColor gray6

        moduleColor : Elm.Module.Name -> Element.Attribute msg
        moduleColor b =
            if active == Just (Just b) then
                noneAttribute

            else
                fontColor gray9
    in
    column [ width fill, height fill ]
        [ link [ width fill, paddingXY 1 0.25, packageColor ]
            { label = text (Elm.Package.toString a.name)
            , url = Router.viewToUrl (Router.PackageView a.name Nothing)
            }
        , Element.Keyed.column [ width fill ]
            (modules
                |> List.map
                    (\v ->
                        ( Elm.Module.toString v
                        , link [ width fill, paddingXY 2.5 0.125, moduleColor v ]
                            { label = text (Elm.Module.toString v)
                            , url = Router.viewToUrl (Router.ModuleView a.name v Nothing)
                            }
                        )
                    )
            )
        , if shortened then
            link [ width fill, paddingXY 2.5 0, fontColor gray9 ]
                { label = text Translation.ellipsis
                , url = Router.viewToUrl (Router.PackageView a.name Nothing)
                }

          else
            none
        ]



--


packagesId =
    "packages-view"


activePackageAndModule : Router.View -> Package.Package -> Maybe (Maybe Elm.Module.Name)
activePackageAndModule view_ a =
    case view_ of
        Router.DefaultView ->
            Nothing

        Router.PackageView b _ ->
            if a.name == b then
                Just Nothing

            else
                Nothing

        Router.ModuleView b c _ ->
            if a.name == b then
                Just (Just c)

            else
                Nothing


filterPackages : String -> List Package.Package -> List Package.Package
filterPackages search a =
    let
        searchWords : List String
        searchWords =
            search |> String.toLower |> String.words

        isRelevant : List String -> Bool
        isRelevant c =
            searchWords |> List.all (\v -> c |> List.any (String.startsWith v))
    in
    a |> List.filter (keywordsFromPackage >> isRelevant)


keywordsFromPackage : Package.Package -> List String
keywordsFromPackage a =
    let
        ( user, project ) =
            case a.name |> Elm.Package.toString |> String.split "/" of
                b :: c :: _ ->
                    ( b, c )

                _ ->
                    ( Elm.Package.toString a.name, "" )
    in
    user
        :: project
        :: (user ++ "/" ++ project)
        :: List.map Elm.Module.toString (Package.exposedToList a.exposed)
        |> List.concatMap toKeywords


toKeywords : String -> List String
toKeywords a =
    let
        keywordsRegex : Regex.Regex
        keywordsRegex =
            Regex.fromString "[A-Z]?[a-z0-9]+" |> Maybe.withDefault Regex.never
    in
    a
        |> Regex.find keywordsRegex
        |> List.map .match
        |> (::) a
        |> List.map String.toLower
