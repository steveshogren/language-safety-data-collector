{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database
       (blankRecord, load, clearFile, updateRecord, clearRepoStats,
        updateRepoBugCount, updateRepoCommitCount, updateRepoName,
        updateRepoNames, lookupLanguage, bug_count, commit_count,
        full_name)
       where

import Records
import qualified Data.Map as M
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as BS
import Control.Lens
import Data.Map.Lens

makeLenses ''Results
makeLenses ''SeveralResults
makeLenses ''RepoStat

blankRecord =
    SeveralResults
    { _languages = []
    }

load :: (Read a) => FilePath -> IO a
load f = read <$> BS.unpack <$> Str.readFile f

save :: (Show a) => a -> FilePath -> IO ()
save x f = Str.writeFile f (BS.pack . show $ x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

updateRecord f aRecord = do
    db <- load f
    save (db & languages <>~ [aRecord]) f

blankRepoStats = (M.fromList [("haskell", M.empty)]) :: RepoStats

clearRepoStats :: String -> IO ()
clearRepoStats f = save blankRepoStats f

makeNewRepo repoName =
    RepoStat
    { _full_name = repoName
    , _bug_count = Nothing
    , _commit_count = Nothing
    }

getRepoStat :: String -> String -> Lens' RepoStats RepoStat
getRepoStat lang repoName =
    at lang . non (M.empty) . at repoName . non (makeNewRepo repoName)

getRepoStatsFromDb f = load f :: IO(RepoStats)

updateRepoFieldCount field f lang repoName count = do
    db <- getRepoStatsFromDb f
    let updated = db & (getRepoStat lang repoName) . field ?~ count
    save updated f

updateRepoBugCount :: FilePath -> String -> String -> Int -> IO ()
updateRepoBugCount = updateRepoFieldCount bug_count

updateRepoCommitCount :: FilePath -> String -> String -> Int -> IO ()
updateRepoCommitCount = updateRepoFieldCount commit_count

addRepoToMap :: RepoStats -> String -> String -> RepoStats
addRepoToMap db lang repoName = db & at lang . non (M.empty) . at repoName ?~ (makeNewRepo repoName)

updateRepoName f lang repoName = do
    db <- getRepoStatsFromDb f
    let x = addRepoToMap db lang repoName
    save x f

updateRepoNames f lang repoNames = do
    db <- getRepoStatsFromDb f
    let x = foldl (\db name -> addRepoToMap db lang name) db repoNames
    save x f

lookupLanguage f lang = do
    db <- getRepoStatsFromDb f
    return $ db ^. at lang

countRepos f lang = do
  l <- lookupLanguage f lang
  return $ M.size <$> l
