{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.PostgreSQL.Simple.Classy.Exceptions.SqlError
       ( AsSqlError    (..)
       , _SqlState
       , _SqlExecStatus
       , _SqlErrorMsg
       , _SqlErrorDetail
       , _SqlErrorHint
       ) where

import Control.Category (id)
import Control.Lens (Getter,Optic',iso,to)

import Data.ByteString (ByteString)

import Data.Functor (Functor)
import Data.Profunctor (Profunctor)

import Database.PostgreSQL.Simple (SqlError (..), ExecStatus)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens ((^?))
-- >>> import Database.PostgreSQL.Simple (ExecStatus (..))
-- >>> import Data.Maybe (Maybe (Just,Nothing))

class AsSqlError p f s where
  _SqlError ::
    Optic' p f s SqlError

instance AsSqlError p f SqlError where
  _SqlError =
    id

-- | An isomorphism on the SqlError record
-- >>> :{
--   ( ("A"::ByteString)
--   , EmptyQuery
--   , ("B"::ByteString)
--   , ("C"::ByteString)
--   , ("D"::ByteString)
--   ) ^? _SqlError :: Maybe SqlError
-- :}
-- Just (SqlError {sqlState = "A", sqlExecStatus = EmptyQuery, sqlErrorMsg = "B", sqlErrorDetail = "C", sqlErrorHint = "D"})
instance (Profunctor p, Functor f)
         => AsSqlError p f ( ByteString
                           , ExecStatus
                           , ByteString
                           , ByteString
                           , ByteString
                           ) where
  _SqlError =
    iso
    (\(a,b,c,d,e) -> SqlError a b c d e)
    (\(SqlError { sqlState       = a
                , sqlExecStatus  = b
                , sqlErrorMsg    = c
                , sqlErrorDetail = d
                , sqlErrorHint   = e
                }
      ) -> (a,b,c,d,e))

-- Some basic getters for a SqlError, no setters because PostgreSQL.Simple will
-- create the error and you shouldn't be fiddling with it.
-- >>> let tstErr = SqlError "A" EmptyQuery "B" "C" "D"
-- >>> tstErr ^. _SqlState
-- "A"
_SqlState :: Getter SqlError ByteString
_SqlState = to (\(SqlError { sqlState = a }) -> a)
-- >>> tstErr ^. _SqlErrorMsg
-- "B"
_SqlErrorMsg :: Getter SqlError ByteString
_SqlErrorMsg = to (\(SqlError { sqlErrorMsg = a }) -> a)
-- >>> tstErr ^. _SqlErrorDetail
-- "C"
_SqlErrorDetail :: Getter SqlError ByteString
_SqlErrorDetail = to (\(SqlError { sqlErrorDetail = a}) -> a)
-- >>> tstErr ^. _SqlErrorHint
-- "D"
_SqlErrorHint :: Getter SqlError ByteString
_SqlErrorHint = to (\(SqlError { sqlErrorHint = a}) -> a)
-- >>> tstErr ^. _SqlExecStatus
-- EmptyQuery
_SqlExecStatus :: Getter SqlError ExecStatus
_SqlExecStatus = to (\(SqlError { sqlExecStatus = a}) -> a)
