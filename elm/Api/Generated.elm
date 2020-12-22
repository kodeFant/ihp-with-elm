module Api.Generated exposing (..)

import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra
import Time


type Widget 
    = BookWidget Book
    | BookSearchWidget 


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        BookWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "BookWidget")
            , ("contents" , bookEncoder b) ]
        
        BookSearchWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "BookSearchWidget")]


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "BookWidget" ->
            Json.Decode.succeed BookWidget |>
            Json.Decode.Pipeline.required "contents" bookDecoder
        
        "BookSearchWidget" ->
            Json.Decode.succeed BookSearchWidget
        
        _ ->
            Json.Decode.fail "No matching constructor")


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