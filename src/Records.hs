{-# LANGUAGE DeriveGeneric #-}
module Records (Results(..)
                , SeveralResults(..)
                , Language
                , RepoStats(..)
                , RepoStat(..)
                ) where

import GHC.Generics
import Data.Aeson
import qualified Data.Map as M

data SeveralResults = SeveralResults {
  _languages :: [Results]
  }
  deriving (Generic, Show, Eq, Read)

data Results = Results {
  _total_count :: Int,
  _language :: String
  }
  deriving (Generic, Show, Eq, Read)

type Language = String
type RepoName = String

type RepoStats = M.Map Language (M.Map RepoName RepoStat)

data RepoStat = RepoStat {
  _full_name :: String,
  _bug_count :: Maybe Int,
  _commit_count :: Maybe Int
  }
  deriving (Generic, Show, Eq, Read)
