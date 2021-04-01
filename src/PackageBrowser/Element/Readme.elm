module PackageBrowser.Element.Readme exposing (..)

import Database.Package.Readme as Readme
import Database.Package.Readme.Decode
import Dict exposing (Dict)
import Element
import Element.Font as Font
import Elm.Docs as Docs
import Elm.Module
import Elm.Module.NameDict as ModuleNameDict
import Elm.Package
import Elm.Package.NameDict as PackageNameDict
import Elm.Type
import Http
import Markdown
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
    { readmes : PackageNameDict.NameDict (Result Error Readme.Readme)
    , expandedSections : PackageNameDict.NameDict (ModuleNameDict.NameDict (Dict String ()))
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { readmes = PackageNameDict.empty
      , expandedSections = PackageNameDict.empty
      }
    , Cmd.none
    )


getPackage : Elm.Package.Name -> Cmd Msg
getPackage a =
    Http.get
        { url = "db/" ++ (a |> Elm.Package.toString |> String.replace "/" " ") ++ ".json"
        , expect = Http.expectJson (GotReadme a) Database.Package.Readme.Decode.readme
        }



--


type Msg
    = ViewChanged
    | GotReadme Elm.Package.Name (Result Http.Error Readme.Readme)
    | Reveal Elm.Package.Name
    | ToggleSection Elm.Package.Name Elm.Module.Name String


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        ViewChanged ->
            case ctx.router.view |> Router.viewToPackageName of
                Just b ->
                    case model.readmes |> PackageNameDict.get b of
                        Just _ ->
                            ( model
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | readmes = model.readmes |> PackageNameDict.insert b (Err Loading) }
                            , getPackage b
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Reveal _ ->
            ( model
            , Cmd.none
            )

        GotReadme a b ->
            ( { model | readmes = model.readmes |> PackageNameDict.insert a (b |> Result.mapError HttpError) }
            , Cmd.none
            )

        ToggleSection a b c ->
            ( { model
                | expandedSections =
                    model.expandedSections
                        |> PackageNameDict.update a
                            (\v ->
                                v
                                    |> Maybe.withDefault ModuleNameDict.empty
                                    |> ModuleNameDict.update b
                                        (\vv ->
                                            vv
                                                |> Maybe.withDefault Dict.empty
                                                |> Dict.update c
                                                    (\vvv ->
                                                        if vvv == Nothing then
                                                            Just ()

                                                        else
                                                            Nothing
                                                    )
                                                |> Just
                                        )
                                    |> Just
                            )
              }
            , Cmd.none
            )



--


view : Router.View -> Model -> Element Msg
view view_ model =
    column
        [ Element.height Element.fill
        , Element.spacing 0
        ]
        (case view_ of
            Router.DefaultView ->
                []

            Router.PackageView b ->
                [ viewPackageHeader b
                , viewPackageReadme (PackageNameDict.get b model.readmes)
                ]

            Router.ModuleView b c ->
                let
                    d : Dict String ()
                    d =
                        model.expandedSections
                            |> PackageNameDict.get b
                            |> Maybe.andThen (ModuleNameDict.get c)
                            |> Maybe.withDefault Dict.empty
                in
                [ viewPackageHeader b
                , viewModuleHeader b c
                , viewModuleReadme b c d (PackageNameDict.get b model.readmes)
                ]
        )


viewReadme : (a -> Element msg) -> Maybe (Result Error a) -> Element msg
viewReadme fn a =
    column
        [ Element.height Element.fill
        , Element.scrollbars
        , Element.paddingEach { left = 16, right = 16, top = 16, bottom = 128 }
        ]
        [ case a of
            Just b ->
                case b of
                    Ok c ->
                        fn c

                    Err c ->
                        case c of
                            Loading ->
                                status []
                                    [ text Strings.loading
                                    ]

                            HttpError d ->
                                status []
                                    [ text (Strings.httpError d)
                                    ]

            Nothing ->
                status []
                    [ text Strings.packageNotFound
                    ]
        ]


viewPackageHeader : Elm.Package.Name -> Element Msg
viewPackageHeader a =
    row
        [ Element.paddingXY 16 12
        , defaultBorderColor
        , borderBottom
        ]
        [ h4 []
            [ link [ defaultTextColor ]
                { label = text (Elm.Package.toString a)
                , url = Router.PackageView a |> Router.viewToUrl
                }
            ]
        , buttonLink []
            { label = text Strings.reveal
            , onPress = Just (Reveal a)
            }
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/"
            }
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a
            }
        ]


viewModuleHeader : Elm.Package.Name -> Elm.Module.Name -> Element msg
viewModuleHeader a b =
    row
        [ Element.paddingXY 16 12
        , defaultBorderColor
        , borderBottom
        ]
        [ h5 []
            [ link [ defaultTextColor ]
                { label = text (Elm.Module.toString b)
                , url = Router.ModuleView a b |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/" ++ (Elm.Module.toString b |> String.replace "." "-")
            }
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a ++ "/blob/master/src/" ++ (Elm.Module.toString b |> String.replace "." "/") ++ ".elm"
            }
        ]


viewPackageReadme : Maybe (Result Error Readme.Readme) -> Element msg
viewPackageReadme a =
    let
        view_ : Readme.Readme -> Element msg
        view_ b =
            p []
                [ Element.html (Markdown.toHtml [] b.readme)
                ]
    in
    viewReadme view_ a



--


viewModuleReadme : Elm.Package.Name -> Elm.Module.Name -> Dict String () -> Maybe (Result Error Readme.Readme) -> Element Msg
viewModuleReadme a b expanded c =
    let
        view_ : Readme.Readme -> Element Msg
        view_ d =
            case d.modules |> ModuleNameDict.get b of
                Just e ->
                    let
                        defaultTitle : String
                        defaultTitle =
                            Elm.Module.toString b
                                |> String.split "."
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                    section []
                        (e
                            |> blocksToSections defaultTitle
                            |> List.map
                                (\( v, vv ) ->
                                    section [ Element.spacing 0 ]
                                        [ buttonLink [ mutedTextColor ]
                                            { label = text v
                                            , onPress = Just (ToggleSection a b v)
                                            }
                                        , column
                                            [ Element.spacing 0
                                            , Element.paddingXY 24 0
                                            ]
                                            (vv |> List.map (viewBlock (Dict.member v expanded)))
                                        ]
                                )
                        )

                Nothing ->
                    status []
                        [ text Strings.moduleNotFound
                        ]
    in
    viewReadme view_ c


blocksToSections : String -> List Docs.Block -> List ( String, List Docs.Block )
blocksToSections defaultTitle a =
    let
        getTitle : Docs.Block -> Maybe String
        getTitle b =
            case b of
                Docs.MarkdownBlock c ->
                    String.lines c
                        |> List.filter (String.startsWith "# ")
                        |> List.head
                        |> Maybe.map (String.dropLeft 2)

                _ ->
                    Nothing

        fold : Docs.Block -> List ( String, List Docs.Block ) -> List ( String, List Docs.Block )
        fold b acc =
            case getTitle b of
                Just c ->
                    ( c, [] ) :: acc

                Nothing ->
                    case acc of
                        [] ->
                            ( defaultTitle, [ b ] ) :: acc

                        ( title, c ) :: rest ->
                            ( title, b :: c ) :: rest
    in
    a
        |> List.foldl fold []
        |> List.map (Tuple.mapSecond List.reverse)
        |> List.reverse


viewBlock : Bool -> Docs.Block -> Element msg
viewBlock expand a =
    case a of
        Docs.MarkdownBlock b ->
            viewBlockItem expand
                { annotation = Nothing
                , docs = b
                }

        Docs.UnionBlock b ->
            viewBlockItem expand
                { annotation = Just { name = b.name, type_ = "" }
                , docs = b.comment
                }

        Docs.AliasBlock b ->
            viewBlockItem expand
                { annotation = Just { name = b.name, type_ = "" :: b.args ++ [ "=" ] ++ typeToString b.tipe |> String.join " " }
                , docs = b.comment
                }

        Docs.ValueBlock b ->
            viewBlockItem expand
                { annotation = Just { name = b.name, type_ = "" :: ":" :: typeToString b.tipe |> String.join " " }
                , docs = b.comment
                }

        Docs.BinopBlock b ->
            viewBlockItem expand
                { annotation = Just { name = "(" ++ b.name ++ ")", type_ = "" :: ":" :: typeToString b.tipe |> String.join " " }
                , docs = b.comment
                }

        Docs.UnknownBlock b ->
            viewBlockItem expand
                { annotation = Nothing
                , docs = b
                }


viewBlockItem : Bool -> { annotation : Maybe { name : String, type_ : String }, docs : String } -> Element msg
viewBlockItem expand a =
    column [ Element.spacing 0 ]
        [ case a.annotation of
            Just { name, type_ } ->
                row [ Element.spacing 0 ]
                    [ text name
                    , el [ Font.color gray500 ] (text type_)
                    ]

            Nothing ->
                none
        , if expand && (a.docs |> String.trim |> String.isEmpty |> not) then
            p [ Element.paddingXY 24 0 ]
                [ Element.html (Markdown.toHtml [] a.docs)
                ]

          else
            none
        ]


typeToString : Elm.Type.Type -> List String
typeToString a =
    case a of
        Elm.Type.Var b ->
            [ b
            ]

        Elm.Type.Lambda b c ->
            [ b |> typeToString |> String.join " "
            , "->"
            , c |> typeToString |> String.join " "
            ]

        Elm.Type.Tuple b ->
            if b == [] then
                [ "()"
                ]

            else
                [ "("
                , b |> List.map (typeToString >> String.join " ") |> String.join ", "
                , ")"
                ]
                    |> String.join " "
                    |> List.singleton

        Elm.Type.Type b c ->
            let
                name : String
                name =
                    b |> String.split "." |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            if c == [] then
                [ name
                ]

            else
                [ name
                , c
                    |> List.map typeToString
                    |> List.map
                        (\v ->
                            if List.length v > 1 then
                                "(" ++ String.join " " v ++ ")"

                            else
                                v |> String.join " "
                        )
                    |> String.join " "
                ]

        Elm.Type.Record b c ->
            let
                open : String
                open =
                    case c of
                        Just d ->
                            "{ " ++ d ++ " |"

                        Nothing ->
                            "{"
            in
            [ open
            , b
                |> List.map (\( v, vv ) -> v :: ":" :: typeToString vv |> String.join " ")
                |> String.join ", "
            , "}"
            ]
                |> String.join " "
                |> List.singleton
