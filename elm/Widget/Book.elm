module Widget.Book exposing (..)

import Api.Generated exposing (Book)
import Html exposing (..)
import Json.Decode as D


type alias Model =
    Book


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view book =
    div []
        [ h2 [] [ text book.title ]
        , p []
            [ text "Pages: "
            , book.pageCount |> String.fromInt |> text
            ]
        , p []
            [ text
                (if book.hasRead == True then
                    "You have read this book"

                 else
                    "You have not read this book"
                )
            ]
        , p [] [ showReview book.review ]
        ]


showReview : Maybe String -> Html msg
showReview maybeReview =
    case maybeReview of
        Just review ->
            text ("Your book review: " ++ review)

        Nothing ->
            text "You have not reviewed this book"