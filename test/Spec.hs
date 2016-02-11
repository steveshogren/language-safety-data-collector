{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import qualified Lib as L
import qualified Database as D
import Records
import Data.Functor ((<$>))

main :: IO ()
main = do
  x <- runTestTT tests
  return ()

dateRange :: Assertion
dateRange =  (L.dateRange @?= " created:\"2011-01-01..2016-01-30\"" )

testDb = "testing"

blankRecord :: Assertion
blankRecord =
   (D.blankRecord @?= (SeveralResults { _languages = []})  )

clearFileTest :: Assertion
clearFileTest = do
  actual <- D.clearFile testDb `seq` (D.load testDb)
  (actual @?= D.blankRecord)

updateFileTest :: Assertion
updateFileTest = do
  actual <- D.clearFile testDb `seq` (D.updateRecord testDb)
  ((_languages actual)  @?= [D.aRecord])

tests = TestList [ "blank record" ~: blankRecord
                   , "date range" ~: dateRange
                   , "update record" ~: updateFileTest
                   , "clearing and reading from a file" ~: clearFileTest]

