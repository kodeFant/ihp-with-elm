module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data BooksController
    = BooksAction
    | NewBookAction
    | ShowBookAction { bookId :: !(Id Book) }
    | CreateBookAction
    | EditBookAction { bookId :: !(Id Book) }
    | UpdateBookAction { bookId :: !(Id Book) }
    | DeleteBookAction { bookId :: !(Id Book) }
    deriving (Eq, Show, Data)
