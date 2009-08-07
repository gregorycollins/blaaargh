{-# LANGUAGE DeriveDataTypeable #-}

module Blaaargh.Internal.Exception 
( BlaaarghException(..)
, blaaarghExceptionMsg )
where

import           Control.Exception
import           Data.Typeable
import           Prelude hiding (catch)


-- | 'BlaaarghException' is the exception type thrown when Blaaargh encounters
-- an error.
data BlaaarghException = BlaaarghException String
  deriving (Show, Typeable)

instance Exception BlaaarghException


-- | Obtain the error message from a 'BlaaarghException'
blaaarghExceptionMsg :: BlaaarghException -> String
blaaarghExceptionMsg (BlaaarghException s) = s
