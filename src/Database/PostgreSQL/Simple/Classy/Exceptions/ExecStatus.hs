{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.PostgreSQL.Simple.Classy.Exceptions.ExecStatus
       ( AsExecStatus (..)
       ) where

import Prelude (Eq,Show (show))

import Data.Profunctor (Profunctor)
import Data.Functor (Functor)

import Data.String (IsString (fromString))
import Data.Maybe (Maybe (Just,Nothing))

import Control.Category (id,(.))
import Control.Lens (Choice,Optic',prism')
import Control.Applicative (Applicative)

import Database.PostgreSQL.Simple (ExecStatus (..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Lens ((^?),(#))
-- >>> import Data.Maybe (Maybe(Just,Nothing))
-- >>> import Data.ByteString (ByteString (..))
-- >>> import Data.String (String)
-- >>> import Data.Text (Text)

class AsExecStatus p f s where
  _ExecStatus ::
    Optic' p f s ExecStatus

instance (Profunctor p, Functor f) => AsExecStatus p f ExecStatus where
  _ExecStatus =
    id

-- |
-- >>> ("TuplesOk"::String) ^? _ExecStatus
-- Just TuplesOk
-- >>> ("CopyOut"::ByteString) ^? _ExecStatus
-- Just CopyOut
-- >>> ("Spanner"::Text) ^? _ExecStatus
-- Nothing
-- >>> ("tuplesok"::String) ^? _ExecStatus
-- Nothing
-- >>> (_ExecStatus # CopyIn) :: String
-- "CopyIn"
-- >>> (_ExecStatus # CopyIn) :: Text
-- "CopyIn"
-- >>> (_ExecStatus # CopyIn) :: ByteString
-- "CopyIn"
instance ( Choice p
         , Applicative f
         , Eq a
         , Show a
         , IsString a
         )
         => AsExecStatus p f a where
  _ExecStatus =
    prism'
    (fromString . show)
    (\s -> case s of
       "EmptyQuery"    -> Just EmptyQuery
       "CommandOk"     -> Just CommandOk
       "TuplesOk"      -> Just TuplesOk
       "CopyOut"       -> Just CopyOut
       "CopyIn"        -> Just CopyIn
       "BadResponse"   -> Just BadResponse
       "NonfatalError" -> Just NonfatalError
       "FatalError"    -> Just FatalError
       _               -> Nothing
    )
