module Web.View.Books.New where
import Web.View.Prelude

data NewView = NewView { book :: Book }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BooksAction}>Books</a></li>
                <li class="breadcrumb-item active">New Book</li>
            </ol>
        </nav>
        <h1>New Book</h1>
        {renderForm book}
    |]

renderForm :: Book -> Html
renderForm book = formFor book [hsx|
    {(textField #title)}
    {(textField #pageCount)}
    {(textField #review)}
    {(checkboxField #hasRead)}
    {(dateField #publishedAt)}
    {submitButton}
|]
