module Web.View.Books.Show where
import Web.View.Prelude

data ShowView = ShowView { book :: Book }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={BooksAction}>Books</a></li>
                <li class="breadcrumb-item active">Show Book</li>
            </ol>
        </nav>
        <h1>Show Book</h1>
        {bookWidget book}
        {bookSearchWidget}
    |]