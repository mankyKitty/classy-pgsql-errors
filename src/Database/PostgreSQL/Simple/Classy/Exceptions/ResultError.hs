{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Database.PostgreSQL.Simple.Classy.Exceptions.ResultError
       ( AsResultError (..)
       , _ErrSQLType
       , _ErrSQLTableOid
       , _ErrHaskellType
       , _ErrSQLField
       , _ErrMessage
       ) where

import Data.String (String)

import Data.Maybe (Maybe)

import Control.Category (id,(.))
import Control.Lens (Getter,Optic',Lens',to,view,_1,_2,_3,_4,_5)

import Database.PostgreSQL.Simple (ResultError (..))
import Database.PostgreSQL.Simple.Types (Oid)

-- $setup
-- >>> import Control.Lens ((^.))
-- >>> import Data.Maybe (Maybe(Just))
-- >>> import Database.PostgreSQL.Simple.Types (Oid (..))
-- >>> import Foreign.C.Types (CUInt (..))

class AsResultError p f s where
  _ResultError ::
    Optic' p f s ResultError

instance AsResultError p f ResultError where
  _ResultError =
    id

-- Oh sweet baby raptors...why is this type built this way. WHY
fromRE :: ResultError -> (String, Maybe Oid, String, String, String)
fromRE (Incompatible { errSQLType     = a
                     , errSQLTableOid = b
                     , errSQLField    = c
                     , errHaskellType = d
                     , errMessage     = e
                     })               = (a,b,c,d,e)
fromRE (UnexpectedNull { errSQLType     = a
                       , errSQLTableOid = b
                       , errSQLField    = c
                       , errHaskellType = d
                       , errMessage     = e
                       })               = (a,b,c,d,e)
fromRE (ConversionFailed { errSQLType     = a
                         , errSQLTableOid = b
                         , errSQLField    = c
                         , errHaskellType = d
                         , errMessage     = e
                         })               = (a,b,c,d,e)

-- Internal helper function. Not for resale.
reFieldGet :: Lens' (String,Maybe Oid,String,String,String) b -> ResultError -> b
reFieldGet l = view l . fromRE

-- |
-- >>> let a = Incompatible ("A"::String) (Just (Oid (CUInt 1))) ("B"::String) ("C"::String) ("D"::String)
-- >>> a ^. _ErrSQLType
-- "A"
-- >>> a ^. _ErrSQLTableOid
-- Just (Oid 1)
-- >>> a ^. _ErrSQLField
-- "B"
-- >>> a ^. _ErrHaskellType
-- "C"
-- >>> a ^. _ErrMessage
-- "D"
_ErrSQLType :: Getter ResultError String
_ErrSQLType = to (reFieldGet _1)

_ErrSQLTableOid :: Getter ResultError (Maybe Oid)
_ErrSQLTableOid = to (reFieldGet _2)

_ErrSQLField :: Getter ResultError String
_ErrSQLField = to (reFieldGet _3)

_ErrHaskellType :: Getter ResultError String
_ErrHaskellType = to (reFieldGet _4)

_ErrMessage :: Getter ResultError String
_ErrMessage = to (reFieldGet _5)
