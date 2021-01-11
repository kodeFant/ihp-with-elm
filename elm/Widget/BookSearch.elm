module Widget.BookSearch exposing (..)

import Api.Generated exposing (Book)
import Api.Http exposing (getBooksAction)
import ErrorView exposing (httpErrorView)
import Html exposing (..)
import Html.Attributes exposing (href, type_)
import Html.Events exposing (onInput)
import Http


type alias Model =
    { searchResult : Result Http.Error (List Book)
    , searchTerm : String
    }


initialModel : Model
initialModel =
    { searchResult = Ok []
    , searchTerm = ""
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = SearchInputChanged String
    | GotSearchResult (Result Http.Error (List Book))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInputChanged text ->
            ( { model | searchTerm = text }, getBooksAction text GotSearchResult )

        GotSearchResult result ->
            ( { model | searchResult = result }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "📚 Search Books 📚" ]
        , input
            [ type_ "search"
            , onInput SearchInputChanged
            ]
            []
        , searchResultView model.searchResult
        ]


searchResultView : Result Http.Error (List Book) -> Html msg
searchResultView searchResult =
    case searchResult of
        Err error ->
            httpErrorView error

        Ok books ->
            ul [] (List.map bookItem books)


bookItem : Book -> Html msg
bookItem book =
    let
        bookLink =
            "/ShowBook?bookId=" ++ book.id
    in
    li [] [ a [ href bookLink ] [ text book.title ] ]