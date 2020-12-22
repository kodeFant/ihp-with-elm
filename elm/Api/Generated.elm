module Api.Generated exposing (..)

import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra
import Time


type Widget 
    = BookWidget Book


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        BookWidget b ->
            bookEncoder b


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.map BookWidget bookDecoder


type alias Book  =
    { id : String
    , title : String
    , pageCount : Int
    , hasRead : Bool
    , review : Maybe String
    , publishedAt : Time.Posix }


bookEncoder : Book -> Json.Encode.Value
bookEncoder a =
    Json.Encode.object [ ("id" , Json.Encode.string a.id)
    , ("title" , Json.Encode.string a.title)
    , ("pageCount" , Json.Encode.int a.pageCount)
    , ("hasRead" , Json.Encode.bool a.hasRead)
    , ("review" , Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.review)
    , ("publishedAt" , Iso8601.encode a.publishedAt) ]


bookDecoder : Json.Decode.Decoder Book
bookDecoder =
    Json.Decode.succeed Book |>
    Json.Decode.Pipeline.required "id" Json.Decode.string |>
    Json.Decode.Pipeline.required "title" Json.Decode.string |>
    Json.Decode.Pipeline.required "pageCount" Json.Decode.int |>
    Json.Decode.Pipeline.required "hasRead" Json.Decode.bool |>
    Json.Decode.Pipeline.required "review" (Json.Decode.nullable Json.Decode.string) |>
    Json.Decode.Pipeline.required "publishedAt" Iso8601.decoder