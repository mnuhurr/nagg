module Main (main) where

import Nagg
import Nagg.DB

main :: IO ()
main = do
  cfg <- readConfig "config.toml"
  
  let db_path = (gcDbPath . cfgGeneral) cfg
  let port = (gcPort . cfgGeneral) cfg
  
  withDB db_path $ \conn -> do
    initDB conn
    runServer conn port

  pure ()
