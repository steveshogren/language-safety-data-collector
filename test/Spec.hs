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
   (D.blankRecord @?= (SeveralResults { languages = []})  )

clearFileTest :: Assertion
clearFileTest =
  D.clearFile testDb
  `seq`
  let actual = D.load testDb
  in (actual) >>= (@?= D.blankRecord)

-- save :: Assertion
-- save = G.saveCount >>= (@?= 2)

-- findAll :: Assertion
-- findAll = G.findAllCount >>= (@?= 2)

-- find :: Assertion
-- find = G.findCount >>= (@?= 2)

tests = TestList [ "blank record" ~: blankRecord
                   , "date range" ~: dateRange]

