{-# LANGUAGE DeriveDataTypeable #-}

module Blaaargh.Exception where

import           Control.Exception
import           Data.Typeable
import           Prelude hiding (catch)



data BlaaarghException = BlaaarghException String
  deriving (Show, Typeable)

instance Exception BlaaarghException
