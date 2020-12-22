module Web.View.Books.Index where
import Web.View.Prelude

data IndexView = IndexView { books :: [Book] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={BooksAction}>Books</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewBookAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Book</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach books renderBook}</tbody>
            </table>
        </div>
    |]


renderBook book = [hsx|
    <tr>
        <td>{book}</td>
        <td><a href={ShowBookAction (get #id book)}>Show</a></td>
        <td><a href={EditBookAction (get #id book)} class="text-muted">Edit</a></td>
        <td><a href={DeleteBookAction (get #id book)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
