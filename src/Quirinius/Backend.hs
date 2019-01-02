module Quirinius.Backend where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)

import Database.Persist.Sql (SqlBackend, rawSql)

import Quirinius.Database
import Quirinius.DSL

runQuery ::
     (ToQuery q, TableRecord tab, QueryResult a, MonadIO m)
  => q tab a
  -> ReaderT SqlBackend m [a]
runQuery q = map deserializeResult <$> rawSql (compileQuery q) []
