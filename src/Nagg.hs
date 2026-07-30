module Nagg
  ( SourceConfig(..)
  , GeneralConfig(..)
  , Config(..)
  , readConfig
  , collectSourceItems
  , runServer
  ) where

import Nagg.API (runServer)
import Nagg.Config
import Nagg.DB
import Nagg.Feed


-- | fetch the items and insert them into the database
collectSourceItems :: FilePath -> SourceConfig -> IO ()
collectSourceItems db_path sc = do
  (si, items) <- fetchSource sc
  -- putStrLn $ "got " <> show (length items) <> " items"
  
  withDB db_path $ \conn -> do
    initDB conn
    
    cid <- ensureCategory conn (scCategory sc)
    sid <- ensureSource conn cid sc si

    insertItems conn sid items
  pure ()
