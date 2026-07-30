{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nagg.DB
  ( withDB
  , initDB
  , insertItems
  , SourceId
  , ensureCategory
  , ensureSource
  ) where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)

import Nagg.Config
import Nagg.Feed

newtype SourceId = SourceId { unSourceId :: Int } 
  deriving (Eq, Show)
  deriving newtype (ToField, FromField)

newtype CategoryId = CategoryId { unCategoryId :: Int } 
  deriving (Eq, Show)
  deriving newtype (ToField, FromField)

-- | general thingy
withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB path = bracket (open path) close

-- | initialize the tables
initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA busy_timeout = 5000"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS category (\
    \  id   INTEGER PRIMARY KEY, \
    \  name TEXT NOT NULL UNIQUE)"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS source (\
    \  id                  INTEGER PRIMARY KEY, \
    \  feed_url            TEXT NOT NULL UNIQUE, \
    \  category_id         INTEGER REFERENCES category(id), \
    \  channel_title       TEXT, \
    \  channel_description TEXT)"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS item (\
    \  id           INTEGER PRIMARY KEY, \
    \  source_id    INTEGER NOT NULL REFERENCES source(id), \
    \  guid         TEXT NOT NULL, \
    \  title        TEXT NOT NULL, \
    \  link         TEXT NOT NULL, \
    \  description  TEXT, \
    \  published_at TEXT NOT NULL, \
    \  fetched_at   TEXT NOT NULL, \
    \  UNIQUE(source_id, guid))"
  execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_item_published ON item(published_at DESC)"

-- | insert news items into the db
insertItems :: Connection -> SourceId -> [NewsItem] -> IO ()
insertItems conn sid items = do
  now <- getCurrentTime
  
  executeMany conn
    "INSERT OR IGNORE INTO item (source_id, guid, title, link, description, published_at, fetched_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?)"
    [(sid, niGuid i, niTitle i, niLink i, niDescription i, niPubDate i, now) | i <- items]


-- | get category id. create a new if doesnt exist
ensureCategory :: Connection -> Text -> IO CategoryId
ensureCategory conn name = do
  existing <- query conn 
    "SELECT id FROM category WHERE name = ?" 
    (Only name)

  case existing of
    (Only cid:_) -> pure cid
    [] -> do
      execute conn 
        "INSERT INTO category (name) VALUES (?)"
        (Only name)

      cid <- lastInsertRowId conn
      pure $ CategoryId (fromIntegral cid)


-- | get source id. craete a new if doesn't exist
ensureSource :: Connection -> CategoryId -> SourceConfig -> SourceInfo -> IO SourceId
ensureSource conn cid sc si = do
  existing <- query conn
    "SELECT id FROM source WHERE feed_url = ?"
    (Only (scUrl sc))

  case existing of
    (Only sid:_) -> pure sid
    [] -> do
      execute conn
        "INSERT INTO source (feed_url, category_id, channel_title, channel_description) \
        \VALUES (?, ?, ?, ?)"
        (scUrl sc, cid, siTitle si, siDescription si)

      sid <- lastInsertRowId conn
      pure $ SourceId (fromIntegral sid)


