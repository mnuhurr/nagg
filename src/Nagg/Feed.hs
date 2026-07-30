{-# LANGUAGE OverloadedStrings #-}
module Nagg.Feed
  ( NewsItem(..)
  , SourceInfo(..)
  , fetchFeed
  , mkNewsItem
  , fetchSource
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Simple (parseRequest, httpSource, getResponseBody, setRequestHeader)
import Text.RSS.Conduit.Parse
import Text.RSS.Types
import Text.XML.Stream.Parse (parseBytes, def, force)
import Conduit

import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.IO (hPutStrLn, stderr)
import URI.ByteString (serializeURIRef')

import Nagg.Config

data SourceInfo = SourceInfo 
  { siTitle :: !Text
  , siDescription :: !Text
  } deriving (Eq, Show)

data NewsItem = NewsItem
  { niGuid :: !Text
  , niTitle :: !Text
  , niLink :: !Text
  , niDescription :: !Text
  , niPubDate :: !UTCTime
  } deriving (Eq, Show)



-- | fetch an rss feed
fetchFeed :: Text -> IO (Either SomeException (RssDocument NoExtensions))
fetchFeed url = try $ runConduitRes $ do
  req0 <- parseRequest (T.unpack url)
  -- let req = setRequestHeader "User-Agent" ["Mozilla/5.0 (compatible; Nagg/0.1)"] req0
  let req = setRequestHeader "User-Agent" ["Nagg/0.1"] req0
  httpSource req getResponseBody
    .| parseBytes def
    .| force "rss parse failed" rssDocument


getGuid :: RssGuid -> Text
getGuid (GuidText t) = t
getGuid (GuidUri uri) = rssUriToText uri

rssUriToText :: RssURI -> Text
rssUriToText (RssURI uri) = TE.decodeUtf8 (serializeURIRef' uri)

mkNewsItem :: UTCTime -> RssItem NoExtensions -> NewsItem
mkNewsItem defaultTime item = NewsItem
  { niGuid = fromMaybe "" $ fmap getGuid (itemGuid item)
  , niTitle = itemTitle item
  , niLink = fromMaybe "" (fmap rssUriToText (itemLink item))
  , niDescription = itemDescription item
  , niPubDate = fromMaybe defaultTime (itemPubDate item)
  }


fetchSource :: SourceConfig -> IO (SourceInfo, [NewsItem])
fetchSource sc = do
  ct <- getCurrentTime
  res <- fetchFeed $ scUrl sc
  case res of
    Left err -> error [] <$ hPutStrLn stderr ("fetch failed: " <> show err)
    Right doc -> do
      let info = SourceInfo 
            { siTitle = channelTitle doc
            , siDescription = channelDescription doc
            }
      let items = map (mkNewsItem ct) (channelItems doc)
      pure (info, items)
