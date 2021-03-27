module Utils.Resolver exposing (..)

import Http exposing (Error(..), Response(..))
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
                    Err (BadBody (Decode.errorToString c))

                Ok c ->
                    Ok c
        )



--


base : (String -> Result Http.Error a) -> Http.Resolver Error a
base fn =
    let
        toResult : Http.Response String -> Result Http.Error a
        toResult a =
            case a of
                BadUrl_ b ->
                    Err (BadUrl b)

                Timeout_ ->
                    Err Timeout

                NetworkError_ ->
                    Err NetworkError

                BadStatus_ b _ ->
                    Err (BadStatus b.statusCode)

                GoodStatus_ _ b ->
                    fn b
    in
    Http.stringResolver toResult
