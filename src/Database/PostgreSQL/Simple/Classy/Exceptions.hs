{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Database.PostgreSQL.Simple.Classy.Exceptions
       ( PgSQLSimpleError (..)
       , AsPgSQLSimpleError (..)
       , module X
       ) where

import           Database.PostgreSQL.Simple.Classy.Exceptions.ExecStatus  as X
import           Database.PostgreSQL.Simple.Classy.Exceptions.FormatError as X
import           Database.PostgreSQL.Simple.Classy.Exceptions.QueryError  as X
import           Database.PostgreSQL.Simple.Classy.Exceptions.ResultError as X
import           Database.PostgreSQL.Simple.Classy.Exceptions.SqlError    as X

import           Data.Functor                                             (Functor)
import           Data.Profunctor                                          (Profunctor)

import           Control.Category                                         (id,
                                                                           (.))
import           Control.Lens                                             (Optic')

import           Database.PostgreSQL.Simple                               ( ExecStatus (..)
                                                                          , FormatError (..)
                                                                          , QueryError (..)
                                                                          , ResultError (..)
                                                                          , SqlError (..)
                                                                          )

data PgSQLSimpleError
  = PgErrorSQL SqlError
  | PgErrorFormat FormatError
  | PgErrorQuery QueryError
  | PgErrorResult ResultError
  | PgStatusExec ExecStatus

class AsPgSQLSimpleError p f s where
  _PgSQLSimpleError ::
    Optic' p f s PgSQLSimpleError

instance AsPgSQLSimpleError p f PgSQLSimpleError where
  _PgSQLSimpleError =
    id

instance (Profunctor p, Functor f) => AsSqlError p f PgSQLSimpleError where
  _SqlError = _PgSQLSimpleError . _SqlError

instance (Profunctor p, Functor f) => AsFormatError p f PgSQLSimpleError where
  _FormatError = _PgSQLSimpleError . _FormatError

instance (Profunctor p, Functor f) => AsQueryError p f PgSQLSimpleError where
  _QueryError = _PgSQLSimpleError . _QueryError

instance (Profunctor p, Functor f) => AsResultError p f PgSQLSimpleError where
  _ResultError = _PgSQLSimpleError . _ResultError

-- This creates an overlapping instance with the prism for ExecStatus.
-- Not sure if this means I have messed up this instance, or my prism is wrong. :<
-- instance (Profunctor p, Functor f) => AsExecStatus p f PgSQLSimpleError where
--   _ExecStatus = _PgSQLSimpleError . _ExecStatus
