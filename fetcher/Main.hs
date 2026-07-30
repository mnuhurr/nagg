module Main (main) where

import Nagg

main :: IO ()
main = do
  cfg <- readConfig "fetcher.toml"
  
  let db_path = (gcDbPath . cfgGeneral) cfg
  let sources = cfgSources cfg

  _ <- mapM (collectSourceItems db_path) sources 
  
  pure ()
