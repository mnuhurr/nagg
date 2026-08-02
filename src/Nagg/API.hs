{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Nagg.API (runServer) where

import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (Connection, query, query_, fromOnly, Only(..), FromRow(..), field)
import Network.Wai.Handler.Warp (run)
import Servant 
  ( type (:>), type (:<|>)(..), Get, JSON, QueryParam, Server, Handler, serve, Raw)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import qualified Data.Text as T

data ItemView = ItemView
  { itemTitle :: !Text
  , itemLink :: !Text
  , itemDescription :: !(Maybe Text)
  , itemPubDate :: !UTCTime
  , itemSourceName :: !Text
  , itemCategory :: !Text
  } deriving (Show, Generic)

instance ToJSON ItemView


type ItemAPI = "items" :> QueryParam "before" UTCTime
                        :> QueryParam "limit" Int
                        :> QueryParam "category" Text
                        :> Get '[JSON] [ItemView]

type CategoriesAPI = "categories" :> Get '[JSON] [Text]

type API = "api" :> (ItemAPI :<|> CategoriesAPI) :<|> Raw


server :: Connection -> Server API
server conn = (getItems conn :<|> getCategories conn) :<|> serveDirectoryFileServer "static"

runServer :: Connection -> Int -> IO ()
runServer conn port = run port (serve (Proxy :: Proxy API) (server conn))


-- | items endpoint
getItems :: Connection -> Maybe UTCTime -> Maybe Int -> Maybe Text -> Handler [ItemView]
getItems conn maybeBfr maybeLimit maybeCat = liftIO $ do
  let limit = fromMaybe 30 maybeLimit
  case maybeBfr of
    Nothing ->
      query conn
        "SELECT item.title, item.link, item.description, item.published_at, source.channel_title, category.name \
        \FROM item \
        \JOIN source ON item.source_id = source.id \
        \JOIN category ON source.category_id = category.id \
        \WHERE (? IS NULL OR category.name = ?) \
        \GROUP BY item.guid \
        \ORDER BY item.published_at DESC LIMIT ?"
        (maybeCat, maybeCat, limit)
    Just before ->
      query conn
        "SELECT item.title, item.link, item.description, item.published_at, source.channel_title, category.name \
        \FROM item \
        \JOIN source ON item.source_id = source.id \
        \JOIN category ON source.category_id = category.id \
        \WHERE item.published_at < ? \
        \  AND (? IS NULL OR category.name = ?) \
        \GROUP BY item.guid \
        \ORDER BY item.published_at DESC LIMIT ?"
        (before, maybeCat, maybeCat, limit)


-- | categories endpoint: just return the list of available categories
getCategories :: Connection -> Handler [Text]
getCategories conn = liftIO $ do
  rows <- query_ conn "SELECT name FROM category ORDER BY name" :: IO [Only Text]
  pure $ map fromOnly rows

instance FromRow ItemView where
  fromRow = ItemView <$> field <*> field <*> field <*> field <*> field <*> (T.toTitle <$> field)

