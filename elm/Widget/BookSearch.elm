module Widget.BookSearch exposing (..)

import Api.Generated exposing (Book)
import Html exposing (..)


type alias Model =
    Result String (List Book)


initialModel : Model
initialModel =
    Ok []


init : Model -> ( Model, Cmd msg )
init model =
    ( model, Cmd.none )


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
view model =
    div []
        [ h2 []
            [ text "🔎 Search Books 🔎" ]
        ]