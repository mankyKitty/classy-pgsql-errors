{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.PostgreSQL.Simple.Classy.Exceptions.QueryError
       ( AsQueryError (..)
       , _QeMessage
       , _QeQuery
       ) where

import Data.String (String)

import Data.Tuple (uncurry)
import Data.Profunctor (Profunctor)
import Data.Functor (Functor)

import Control.Category (id)
import Control.Lens (Getter,Optic',iso,to)

import Database.PostgreSQL.Simple (Query,QueryError (..))

-- Not sure about the FlexibleContexts here...

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Control.Lens ((^?),(^.),from,(#))
-- >>> import Data.Maybe (Maybe(Just,Nothing))
-- >>> import Database.PostgreSQL.Simple (Query (..))
-- >>> let fakeQ = ("select foo from bar where buzz" :: Query)

-- Query is an instance of `IsString` and is a `ByteString` internally.

class AsQueryError p f s where
  _QueryError ::
    Optic' p f s QueryError

instance AsQueryError p f QueryError where
  _QueryError =
    id

-- |
-- >>> ("Foof"::String, "Select *" :: Query) ^. _QueryError
-- QueryError {qeMessage = "Foof", qeQuery = "Select *"}
-- >>> (_QueryError # QueryError ("Foof"::String) ("Select *"::Query)) :: (String,Query)
-- ("Foof","Select *")
instance (Profunctor p, Functor f) => AsQueryError p f (String,Query) where
  _QueryError =
    iso
    (uncurry QueryError)
    (\(QueryError { qeMessage = a, qeQuery = b}) -> (a,b))

-- |
-- >>> QueryError {qeMessage = "Foof", qeQuery = "Select *"} ^. _QeMessage
-- "Foof"
-- >>> QueryError {qeMessage = "Foof", qeQuery = "Select *"} ^. _QeQuery
-- "Select *"
_QeMessage :: Getter QueryError String
_QeMessage = to (\(QueryError { qeMessage = a}) -> a)

_QeQuery :: Getter QueryError Query
_QeQuery = to (\(QueryError { qeQuery = a}) -> a)
