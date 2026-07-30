module Nagg
  ( SourceConfig(..)
  , GeneralConfig(..)
  , Config(..)
  , readConfig
  , collectSourceItems
  , parseFile
  , dumpResp
  ) where

import Database.SQLite.Simple

import Nagg.Config
import Nagg.DB
import Nagg.Feed


-- debug
import qualified Data.ByteString as BS
import Conduit
import Text.XML.Stream.Parse (parseBytes, def, force)
import Text.RSS.Conduit.Parse (rssDocument)
import Text.RSS.Types

import Network.HTTP.Simple


-- | debug
parseFile :: FilePath -> IO (RssDocument NoExtensions)
parseFile fp = do
  bytes <- BS.readFile fp
  runConduit $ 
    yield bytes 
      .| parseBytes def
      .| force "rss parse failed" rssDocument


dumpResp :: String -> IO ()
dumpResp url = do
  resp <- httpBS (parseRequest_ url)
  print $ getResponseStatus resp
  print $ getResponseHeaders resp
  pure ()


-- | fetch the items and insert them into the database
collectSourceItems :: Connection -> SourceConfig -> IO ()
collectSourceItems conn sc = do
  cid <- ensureCategory conn (scCategory sc)
  (si, items) <- fetchSource sc
  -- putStrLn $ "got " <> show (length items) <> " items"
  sid <- ensureSource conn cid sc si
  insertItems conn sid items
