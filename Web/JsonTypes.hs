{-# language DeriveAnyClass #-}

module Web.JsonTypes where

import Generated.Types
import IHP.ControllerPrelude
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )

-- JSON serializable types and functions
-- for exposing IHP data to Elm and JSON responses

data BookJSON = BookJSON
  { id :: Text
  , title :: Text
  , pageCount :: Int
  , hasRead :: Bool
  , review :: Maybe Text
  , publishedAt :: UTCTime
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.Book" BookJSON

bookToJSON :: Book -> BookJSON
bookToJSON book =
    BookJSON {
        id = show $ get #id book,
        title = get #title book,
        pageCount = get #pageCount book,
        hasRead = get #hasRead book,
        review = get #review book,
        publishedAt = get #publishedAt book
    }