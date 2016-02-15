{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database
       (blankRecord, load, clearFile, updateRecord, clearRepoStats,
        updateRepoBugCount, updateRepoCommitCount, updateRepoName,
        lookupLanguage, bug_count, commit_count, full_name)
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

updateRepoFieldCount field f lang repoName count = do
    db <- load f :: IO (RepoStats)
    let updated = db & (getRepoStat lang repoName) . field ?~ count
    save updated f

updateRepoBugCount :: FilePath -> String -> String -> Int -> IO ()
updateRepoBugCount = updateRepoFieldCount bug_count

updateRepoCommitCount :: FilePath -> String -> String -> Int -> IO ()
updateRepoCommitCount = updateRepoFieldCount commit_count

updateRepoName f lang repoName = do
    db <- load f :: IO (RepoStats)
    let x = db & at lang . non (M.empty) . at repoName ?~ (makeNewRepo repoName)
    save x f

lookupLanguage f lang = do
    db <- load f :: IO (RepoStats)
    return $ db ^. at lang

countRepos f lang = do
  l <- lookupLanguage f lang
  return $ M.size <$> l
