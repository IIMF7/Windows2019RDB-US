module Utils.Resolver exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)


string : Http.Resolver Http.Error String
string =
    base Ok


json : Decoder a -> Http.Resolver Http.Error a
json decoder =
    base
        (\b ->
            case b |> Decode.decodeString decoder of
                Err c ->
                    Err (Http.BadBody (Decode.errorToString c))

                Ok c ->
                    Ok c
        )



--


base : (String -> Result Http.Error a) -> Http.Resolver Http.Error a
base fn =
    let
        toResult : Http.Response String -> Result Http.Error a
        toResult a =
            case a of
                Http.BadUrl_ b ->
                    Err (Http.BadUrl b)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ b _ ->
                    Err (Http.BadStatus b.statusCode)

                Http.GoodStatus_ _ b ->
                    fn b
    in
    Http.stringResolver toResult
