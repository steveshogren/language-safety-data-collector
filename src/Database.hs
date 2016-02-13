{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database (blankRecord
                , load
                , clearFile
                , updateRecord
               ) where

import Records
import qualified Data.Map as M
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as BS
import Control.Lens
import Data.Map.Lens
import qualified Data.Map as M

makeLenses ''Results
makeLenses ''SeveralResults
makeLenses ''RepoStat

blankRecord = SeveralResults { _languages = [] }

load :: (Read a) => FilePath -> IO a
load f = read <$> BS.unpack <$> Str.readFile f

save :: (Show a) => a -> FilePath -> IO ()
save x f = Str.writeFile f (BS.pack . show $ x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

updateRecord f aRecord = do
  db <- load f
  save (db & languages <>~ [aRecord]) f

blankRepoStats = (M.empty::RepoStats)

clearRepoStats :: String -> IO ()
clearRepoStats f = save blankRepoStats f

makeNewRepo repoName =
   RepoStat{_full_name = repoName, _bug_count = Nothing, _commit_count = Nothing}

saveRepoStatName f repoName = do
  db <- load f :: IO(RepoStats)
  let updated = (ix "haskell" <>~ [makeNewRepo repoName]) $ db
  save updated f
  return ()

lensIt = do
  db <- load "repostats" :: IO(RepoStats)
  return $ db ^.at "haskell"
