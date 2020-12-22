module Web.View.Books.Edit where
import Web.View.Prelude

data EditView = EditView { book :: Book }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BooksAction}>Books</a></li>
                <li class="breadcrumb-item active">Edit Book</li>
            </ol>
        </nav>
        <h1>Edit Book</h1>
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
