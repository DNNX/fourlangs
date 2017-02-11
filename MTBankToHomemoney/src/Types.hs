{-# LANGUAGE OverloadedStrings #-}

module Types
  ( HMRecord(..),
    MTRecord(..)
  ) where

import Data.Csv

data HMRecord = HMRecord {} deriving (Show)

data MTRecord = MTRecord
  { hmType :: !String
  } deriving (Show)

instance FromNamedRecord MTRecord where
  parseNamedRecord r = MTRecord <$> r .: "Тип"

instance ToNamedRecord HMRecord where
  toNamedRecord _ = namedRecord []

instance DefaultOrdered HMRecord where
  headerOrder _ = header []
