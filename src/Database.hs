{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database
       (blankRecord, load,loadT, clearFile, clearFileT, updateRecord, updateRecordT, clearRepoStats,
        clearRepoStatsT,
        updateRepoBugCount, updateRepoCommitCount, updateRepoName,
        updateRepoBugCountT, updateRepoCommitCountT, updateRepoNameT,
        updateRepoNames, lookupLanguage, bug_count, commit_count,
        updateRepoNamesT, 
        lookupLanguageT,
        full_name)
       where

import Records
import qualified Data.Map as M
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as BS
import Control.Lens
import Data.Map.Lens
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask, local)

makeLenses ''Results
makeLenses ''SeveralResults
makeLenses ''RepoStat

blankRecord =
    SeveralResults
    { _languages = []
    }

loadT :: (Read a) => ReaderT FilePath IO a
loadT = do
  fileName <- ask
  liftIO $ read <$> BS.unpack <$> Str.readFile fileName

saveT :: (Show a) => a -> ReaderT FilePath IO ()
saveT x = do
  fileName <- ask
  liftIO $ Str.writeFile fileName (BS.pack . show $ x)

clearFileT :: ReaderT FilePath IO ()
clearFileT = saveT blankRecord

updateRecordT :: Results -> ReaderT FilePath IO ()
updateRecordT aRecord = do
    db <- loadT
    saveT (db & languages <>~ [aRecord])

clearRepoStatsT :: ReaderT FilePath IO ()
clearRepoStatsT = saveT blankRepoStats

getRepoStatsFromDbT :: ReaderT FilePath IO RepoStats
getRepoStatsFromDbT = loadT

updateRepoFieldCountT :: ((a -> Identity (Maybe b)) -> RepoStat -> Identity RepoStat)
                      -> String -> String -> b -> ReaderT FilePath IO ()
updateRepoFieldCountT field lang repoName count = do
    db <- getRepoStatsFromDbT
    let updated = db & (getRepoStat lang repoName) . field ?~ count
    saveT updated

updateRepoBugCountT :: String -> String -> Int -> ReaderT FilePath IO ()
updateRepoBugCountT = updateRepoFieldCountT bug_count

updateRepoCommitCountT :: String -> String -> Int -> ReaderT FilePath IO ()
updateRepoCommitCountT = updateRepoFieldCountT commit_count

updateRepoNameT :: String -> String -> ReaderT FilePath IO ()
updateRepoNameT lang repoName = do
    db <- getRepoStatsFromDbT
    let x = addRepoToMap db lang repoName
    saveT x

updateRepoNamesT :: String -> [String] -> ReaderT FilePath IO ()
updateRepoNamesT lang repoNames = do
    db <- getRepoStatsFromDbT
    let x = foldl (\db name -> addRepoToMap db lang name) db repoNames
    saveT x

lookupLanguageT lang = do
    db <- getRepoStatsFromDbT
    return $ db ^. at lang

countReposT lang = do
  l <- lookupLanguageT lang
  return $ M.size <$> l

-- lame old passing FilePath way
load :: (Read a) => FilePath -> IO a
load f = read <$> BS.unpack <$> Str.readFile f

save :: (Show a) => a -> FilePath -> IO ()
save x f = Str.writeFile f (BS.pack . show $ x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

updateRecord :: FilePath -> Results -> IO ()
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

getRepoStatsFromDb :: FilePath -> IO RepoStats
getRepoStatsFromDb f = load f :: IO(RepoStats)

updateRepoFieldCount :: ((a -> Identity (Maybe b)) -> RepoStat -> Identity RepoStat)
     -> FilePath -> String -> String -> b -> IO ()
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

updateRepoName :: FilePath -> String -> String -> IO ()
updateRepoName f lang repoName = do
    db <- getRepoStatsFromDb f
    let x = addRepoToMap db lang repoName
    save x f

updateRepoNames :: FilePath -> String -> [String] -> IO ()
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
