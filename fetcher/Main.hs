module Main (main) where

import Nagg
import Nagg.DB (withDB, initDB)

main :: IO ()
main = do
  cfg <- readConfig "fetcher.toml"
  
  let db_path = (gcDbPath . cfgGeneral) cfg
  withDB db_path $ \conn -> do
    initDB conn

    _ <- mapM (collectSourceItems conn) $ cfgSources cfg
    pure ()
  
  pure ()
