{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- This file demonstrates using Quirinius with a MySQL backend.

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.MySQL
  ( ConnectInfo(..)
  , defaultConnectInfo
  , withMySQLPool
  )
import Database.Persist.Sql (liftSqlPersistMPool, rawExecute, toPersistValue)

import Quirinius



--------------------------------------------------------------------------------
-- * Examples
--------------------------------------------------------------------------------

data Person = Person
  { name   :: Text
  , age    :: Int
  , height :: Double
  , drives :: Text
  } deriving (Eq, Show, Generic)

instance TableRecord Person
instance QueryResult Person

data Car = Car
  { brand :: Text
  , speed :: Double
  } deriving (Eq, Show, Generic)

instance TableRecord Car
instance QueryResult Car

-- Restriction and selection
q1 :: Query Person (Record '["name" :-> Text])
q1 =
  table @Person &
  where_ (#age .> 50) &
  where_ (#height .> 1.75) &
  select @'["name"]

-- Return a text value rather than a singleton record
q2 :: Query Person Text
q2 =
  table @Person &
  where_ (#age .> 50) &
  where_ (#height .> 1.75) &
  project #name

-- Summation
q3 :: Query Person Double
q3 =
  table @Person &
  where_ (#age .> 20) &
  sum_ (#height * 2)

-- Full table (the result is a `Person`)
q4 :: RowQuery Person Person
q4 = table @Person

-- Sliced table (the result is an anonymous record)
q5 :: Query Person (Record '["name" :-> Text, "age" :-> Int])
q5 =
  table @Person &
  select @'["name", "age"]

-- Inner query
fastDrivers :: Query Person Text
fastDrivers =
  table @Person &
  where_ (#drives .= inner fastCars) &
  project #name
  where
    fastCars =
      table @Car &
      where_ (#speed .> 120) &
      project #brand



--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

-- The Quirinius DSL is restricted to SELECT queries. Therefore, we will use
-- explicit "raw" queries to populate the database below.

insertPerson Person {..} =
  rawExecute
    "INSERT IGNORE INTO person(name,age,height,drives) VALUES (?,?,?,?)"
    [ toPersistValue name
    , toPersistValue age
    , toPersistValue height
    , toPersistValue drives
    ]

insertCar Car {..} =
  rawExecute
    "INSERT IGNORE INTO car(brand,speed) VALUES (?,?)"
    [toPersistValue brand, toPersistValue speed]

conn = defaultConnectInfo
  { connectHost = "localhost"
  , connectDatabase = "quirinius_test"
  , connectUser = "quirinius_test"
  }

main = do
  -- Show generated SQL
  Text.putStrLn $ compileQuery q1
  Text.putStrLn $ compileQuery q2
  Text.putStrLn $ compileQuery q3
  Text.putStrLn $ compileQuery q4
  Text.putStrLn $ compileQuery q5
  Text.putStrLn $ compileQuery fastDrivers

  -- Run queries
  runNoLoggingT $ withMySQLPool conn 10 $ liftSqlPersistMPool $ do
    -- (Use `runNoLoggingT` to disable logging.)

    -- Ensure a clean database
    rawExecute "CREATE TABLE IF NOT EXISTS person(name TEXT, age INT, height DOUBLE, drives TEXT);" []
    rawExecute "CREATE TABLE IF NOT EXISTS car(brand TEXT, speed DOUBLE);" []
    rawExecute "DELETE FROM person" []
    rawExecute "DELETE FROM car" []

    -- Insert data
    mapM_ insertPerson
      [ Person "Arne" 55 1.82 "Volvo"
      , Person "Anna" 44 1.72 "Volvo"
      , Person "Bror" 33 1.62 "SAAB"
      ]

    mapM_ insertCar
      [ Car "SAAB"  120
      , Car "Volvo" 125
      ]

    -- Run Quirinius queries
    liftIO $ putStrLn "\n--- q1:"
    mapM_ (liftIO . print) =<< runQuery q1

    liftIO $ putStrLn "\n--- q2:"
    mapM_ (liftIO . print) =<< runQuery q2

    liftIO $ putStrLn "\n--- q3:"
    mapM_ (liftIO . print) =<< runQuery q3

    liftIO $ putStrLn "\n--- q4:"
    mapM_ (liftIO . print) =<< runQuery q4

    liftIO $ putStrLn "\n--- q5:"
    mapM_ (liftIO . print) =<< runQuery q5

    liftIO $ putStrLn "\n--- fastDrivers:"
    mapM_ (liftIO . print) =<< runQuery fastDrivers
