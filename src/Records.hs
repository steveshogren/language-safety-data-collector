{-# LANGUAGE DeriveGeneric #-}
module Records (Results
                , SeveralResults(..)
                , Lang
                , LangStats
                ) where

import GHC.Generics
import Data.Aeson

data SeveralResults = SeveralResults {
  languages :: [Results]
  }
  deriving (Generic, Show, Eq)

instance ToJSON SeveralResults
instance FromJSON SeveralResults

data Results = Results {
  total_count :: Int,
  language :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Results
instance FromJSON Results

data LangStats = LangStats {
  langs :: [Lang]
  }
  deriving (Generic, Show)

instance ToJSON LangStats
instance FromJSON LangStats

data Lang = Lang {
  full_name :: String,
  bug_count :: Int
  }
  deriving (Generic, Show)

instance ToJSON Lang
instance FromJSON Lang
