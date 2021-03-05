{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Sqlite.Test.Select (tests) where

import Data.Int (Int32)

import Database.Beam
import Database.Beam.Sqlite
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime, Day(..), UTCTime(..), fromGregorian, getCurrentTime, secondsToDiffTime)

import Database.Beam
import Database.Beam.Sqlite

import Database.Beam.Sqlite.Test

tests :: TestTree
tests = testGroup "Selection tests"
  [ expectFail testExceptValues
  , testInRowValues
  , testInSelect
  ]

data Pair f = Pair
  { _left :: C f Bool
  , _right :: C f Bool
  } deriving (Generic, Beamable)

testInRowValues :: TestTree
testInRowValues = testCase "IN with row values works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $ do
      let p :: forall ctx s. Pair (QGenExpr ctx Sqlite s)
          p = val_ $ Pair False False
      return $ p `in_` [p, p]
    assertEqual "result" [True] result

testInSelect :: TestTree
testInSelect = testCase "IN (SELECT ...) works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $ do
      let x  = as_ @Int32 (val_ 1)
      return $ x `inQuery_` (pure (as_ @Int32 $ val_ 1))
    assertEqual "result" [True] result

    result <- runBeamSqlite conn $ runSelectReturningList $ select $ do
      let x  = as_ @Int32 (val_ 1)
      return $ x `inQuery_`  (pure (as_ @Int32 $ val_ 2))
    assertEqual "result" [False] result

-- | Regression test for <https://github.com/haskell-beam/beam/issues/326 #326>
testExceptValues :: TestTree
testExceptValues = testCase "EXCEPT with VALUES works" $
  withTestDb $ \conn -> do
    result <- runBeamSqlite conn $ runSelectReturningList $ select $
      values_ [as_ @Bool $ val_ True, val_ False] `except_` values_ [val_ False]
    assertEqual "result" [True] result

data TestTableT f
  = TestTable
  { ttId :: C f Int32
  , ttFirstName :: C f Text
  , ttLastName  :: C f Text
  , ttAge       :: C f Int32
  , ttDateJoined :: C f LocalTime
  , ttDateLoggedIn :: C f UTCTime
  } deriving (Generic, Beamable)

deriving instance Show (TestTableT Identity)
deriving instance Eq (TestTableT Identity)

instance Table TestTableT where
  data PrimaryKey TestTableT f = TestTableKey (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = TestTableKey <$> ttId

data TestTableDb entity
  = TestTableDb
  { dbTestTable :: entity (TableEntity TestTableT)
  } deriving (Generic, Database Sqlite)

testDatabase :: DatabaseSettings be TestTableDb
testDatabase = defaultDbSettings

testInsertReturningColumnOrder :: TestTree
testInsertReturningColumnOrder = testCase "runInsertReturningList with mismatching column order" $ do
  now <- getCurrentTime
  let zeroUtcTime = UTCTime (ModifiedJulianDay 0) 0
  let oneUtcTime = UTCTime (fromGregorian 1 0 0) (secondsToDiffTime 0)

  withTestDb $ \conn -> do
    execute_ conn "CREATE TABLE test_table ( date_joined TIMESTAMP NOT NULL, date_logged_in TIMESTAMP WITH TIME ZONE NOT NULL, first_name TEXT NOT NULL, id INT PRIMARY KEY, age INT NOT NULL, last_name TEXT NOT NULL )"
    inserted <-
      runBeamSqlite conn $ runInsertReturningList $
      insert (dbTestTable testDatabase) $
      insertExpressions [ TestTable 0 (concat_ [ "j", "im" ]) "smith" 19 currentTimestamp_ (val_ zeroUtcTime)
                        , TestTable 1 "sally" "apple" ((val_ 56 + val_ 109) `div_` 5) currentTimestamp_ (val_ oneUtcTime)
                        , TestTable 4 "blah" "blah" (-1) currentTimestamp_ (val_ now) ]

    let dateJoined = ttDateJoined (head inserted)

        expected = [ TestTable 0 "jim" "smith" 19 dateJoined zeroUtcTime
                   , TestTable 1 "sally" "apple" 33 dateJoined oneUtcTime
                   , TestTable 4 "blah" "blah" (-1) dateJoined now ]

    assertEqual "insert values" inserted expected
