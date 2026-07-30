{-# LANGUAGE OverloadedStrings #-}
module Nagg.Config 
  ( GeneralConfig(..)
  , SourceConfig(..)
  , Config(..)
  , readConfig
  ) where


import Data.Maybe (fromMaybe)
import Data.Text (Text)
import TOML (DecodeTOML, tomlDecoder, getField, getFieldOpt, decodeFile)

data GeneralConfig = GeneralConfig
  { gcDbPath :: FilePath
  , gcPort :: Int
  } deriving (Eq, Show)

data SourceConfig = SourceConfig
  { scUrl :: Text
  , scCategory :: Text
  } deriving (Eq, Show)

data Config = Config
  { cfgGeneral :: GeneralConfig
  , cfgSources :: [SourceConfig]
  } deriving (Eq, Show)


instance DecodeTOML GeneralConfig where
  tomlDecoder = 
    GeneralConfig 
      <$> getField "db_path"
      <*> (fromMaybe 8080 <$> getFieldOpt "port")

instance DecodeTOML SourceConfig where
  tomlDecoder = 
    SourceConfig
      <$> getField "url"
      <*> getField "category"

instance DecodeTOML Config where
  tomlDecoder =
    Config
      <$> getField "general"
      <*> getField "source"

readConfig :: FilePath -> IO Config
readConfig fp = do
  res <- decodeFile fp
  case res of
    Right cfg -> pure cfg
    Left _ -> error "error reading config"
