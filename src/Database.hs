{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database
       (blankRecord, loadT, clearFileT, updateRecordT, clearRepoStatsT,
        updateRepoBugCountT, updateRepoCommitCountT, updateRepoNameT,
        bug_count, commit_count, updateRepoNamesT, lookupLanguageT,
        QueryDb, MutateDb, full_name)
       where

import Records
import qualified Data.Map as M
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as BS
import Control.Lens
import Data.Map.Lens
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask)

type QueryDb a = ReaderT FilePath IO a
type MutateDb = ReaderT FilePath IO ()
type Repository = String

makeLenses ''Results
makeLenses ''SeveralResults
makeLenses ''RepoStat

blankRecord =
    SeveralResults
    { _languages = []
    }

loadT :: (Read a) => QueryDb a
loadT = do
  fileName <- ask
  liftIO $ read <$> BS.unpack <$> Str.readFile fileName

saveT :: (Show a) => a -> MutateDb
saveT x = do
  fileName <- ask
  liftIO $ Str.writeFile fileName (BS.pack . show $ x)

clearFileT :: MutateDb
clearFileT = saveT blankRecord

updateRecordT :: Results -> MutateDb
updateRecordT aRecord = do
    db <- loadT
    saveT (db & languages <>~ [aRecord])

clearRepoStatsT :: MutateDb
clearRepoStatsT = saveT blankRepoStats

getRepoStatsFromDbT :: QueryDb RepoStats
getRepoStatsFromDbT = loadT

updateRepoFieldCountT :: ((a -> Identity (Maybe b)) -> RepoStat -> Identity RepoStat)
                      -> Language -> Repository -> b -> MutateDb
updateRepoFieldCountT field lang repoName count = do
    db <- getRepoStatsFromDbT
    let updated = db & (getRepoStat lang repoName) . field ?~ count
    saveT updated

updateRepoBugCountT :: Language -> Repository -> Int -> MutateDb
updateRepoBugCountT = updateRepoFieldCountT bug_count

updateRepoCommitCountT :: Language -> Repository -> Int -> MutateDb
updateRepoCommitCountT = updateRepoFieldCountT commit_count

updateRepoNameT :: Language -> Repository -> MutateDb
updateRepoNameT lang repoName = do
    db <- getRepoStatsFromDbT
    let x = addRepoToMap db lang repoName
    saveT x

updateRepoNamesT :: Language -> [Repository] -> MutateDb
updateRepoNamesT lang repoNames = do
    db <- getRepoStatsFromDbT
    let x = foldl (\db name -> addRepoToMap db lang name) db repoNames
    saveT x

lookupLanguageT lang = do
    db <- getRepoStatsFromDbT
    return $ db ^. at lang

countReposT :: Language -> QueryDb (Maybe Int)
countReposT lang = do
  l <- lookupLanguageT lang
  return $ M.size <$> l

blankRepoStats = (M.fromList [("haskell", M.empty)]) :: RepoStats

makeNewRepo repoName =
    RepoStat
    { _full_name = repoName
    , _bug_count = Nothing
    , _commit_count = Nothing
    }

getRepoStat :: Language -> Repository -> Lens' RepoStats RepoStat
getRepoStat lang repoName =
    at lang . non (M.empty) . at repoName . non (makeNewRepo repoName)

addRepoToMap :: RepoStats -> Language -> Repository -> RepoStats
addRepoToMap db lang repoName = db & at lang . non (M.empty) . at repoName ?~ (makeNewRepo repoName)
