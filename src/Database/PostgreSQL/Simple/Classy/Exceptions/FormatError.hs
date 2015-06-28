{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.PostgreSQL.Simple.Classy.Exceptions.FormatError
       ( AsFormatError (..)
       , _FmtMessage
       , _FmtQuery
       , _FmtParams
       ) where

import Data.String (String)
import Data.ByteString (ByteString)

import Data.Profunctor (Profunctor)
import Data.Functor (Functor)

import Control.Category (id)
import Control.Lens (Getter,Optic',iso,to)

import Database.PostgreSQL.Simple (Query,FormatError (..))

-- $setup
-- >>> import Control.Lens ((^?),(^.))
-- >>> import Data.Maybe (Maybe(Just,Nothing))
-- >>> import Database.PostgreSQL.Simple (Query (..))
-- >>> let fakeQ = ("select foo from bar where buzz" :: Query)

-- Query is an instance of `IsString` and is a `ByteString` internally.

class AsFormatError p f s where
  _FormatError ::
    Optic' p f s FormatError

instance AsFormatError p f FormatError where
  _FormatError =
    id

-- |
-- >>> (("fuzz"::String), fakeQ, ["foo"::ByteString,"bar"]) ^? _FormatError
-- Just (FormatError {fmtMessage = "fuzz", fmtQuery = "select foo from bar where buzz", fmtParams = ["foo","bar"]})
instance (Profunctor p, Functor f) => AsFormatError p f (String, Query, [ByteString]) where
  _FormatError =
    iso
    (\(a,b,c) -> FormatError a b c)
    (\(FormatError { fmtMessage = a
                   , fmtQuery = b
                   , fmtParams = c}
      ) -> (a,b,c))

-- |
-- >>> let a = FormatError ("Oh noes"::String) fakeQ ["Fuzz"::ByteString,"Bozz"]
-- >>> a ^. _FmtMessage
-- "Oh noes"
-- >>> a ^. _FmtQuery
-- "select foo from bar where buzz"
-- >>> a ^. _FmtParams
-- ["Fuzz","Bozz"]
_FmtMessage :: Getter FormatError String
_FmtMessage = to (\(FormatError { fmtMessage = a}) -> a)

_FmtQuery :: Getter FormatError Query
_FmtQuery = to (\(FormatError { fmtQuery = a}) -> a)

_FmtParams :: Getter FormatError [ByteString]
_FmtParams = to (\(FormatError { fmtParams = a}) -> a)
