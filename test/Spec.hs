{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import qualified Lib as L
import qualified Database as D
import Control.Lens
import Records
import Data.Functor ((<$>))

makeLenses ''Results
makeLenses ''SeveralResults

main :: IO ()
main = do
  x <- runTestTT tests
  return ()

dateRange :: Assertion
dateRange =  (L.dateRange @?= " created:\"2011-01-01..2016-01-30\"" )

testDb = "testing"

blankRecord :: Assertion
blankRecord =
   (D.blankRecord^.languages @?= [])

clearFileTest :: Assertion
clearFileTest = do
  actual <- D.clearFile testDb >> D.load testDb
  (actual @?= D.blankRecord)

aRecord = Results { _total_count = 5, _language = "Haskell"}

updateFileTest :: Assertion
updateFileTest = do
  actual <- D.clearFile testDb >> D.updateRecord testDb aRecord >> D.load testDb
  ((actual^.languages) @?= [aRecord])

makeNewRepo repoName =
   RepoStat{_full_name = repoName, _bug_count = Nothing, _commit_count = Nothing}

saveFieldCount :: Assertion
saveFieldCount = do
  actual <- D.clearRepoStats testDb
              >> D.updateRepoBugCount testDb "haskell" "somerepo" 2
              >> D.updateRepoCommitCount testDb "haskell" "somerepo" 4
              >> D.lensIt testDb "haskell"
  ((actual ^.at "somerepo" . non (makeNewRepo "t") . (D.bug_count)) @?= Just 2)
  ((actual ^.at "somerepo" . non (makeNewRepo "t") . (D.commit_count)) @?= Just 4)

tests = TestList ["blank record" ~: blankRecord
                , "date range" ~: dateRange
                , "update record" ~: updateFileTest
                , "saving repo stats" ~: saveFieldCount
                , "clearing and reading from a file" ~: clearFileTest]


