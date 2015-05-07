{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Hours where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Time (TimeOfDay(..), LocalTime(..), Day(..))
import Database.Persist.Sql
import Data.Aeson
import Control.Monad

data WeekDay = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving (Show, Read, Eq, Ord)

instance ToJSON WeekDay where
  toJSON = String . pack . show

instance FromJSON WeekDay where
  parseJSON (String t) = pure . read $ unpack t
  parseJSON _ = mzero

data Hours = Hours (M.Map WeekDay (TimeOfDay, TimeOfDay)) deriving (Show, Read, Eq)

instance ToJSON TimeOfDay where
    toJSON (TimeOfDay hour min sec) = object [ "hour" .= hour
                                           , "minute" .= min
                                           , "second" .= sec
                                           ]

instance FromJSON TimeOfDay where
  parseJSON (Object v) = TimeOfDay <$>
                         v .: "hour" <*>
                         v .: "minute" <*>
                         v .: "second"
  parseJSON _ = mzero

instance ToJSON LocalTime where
  toJSON (LocalTime (ModifiedJulianDay day) time) =
    object [ "day" .= day
           , "time" .= time
           ]

instance FromJSON LocalTime where
  parseJSON (Object v) = LocalTime <$>
                           ModifiedJulianDay <$> v .: "day" <*>
                           v .: "time"


instance ToJSON Hours where
  toJSON (Hours m) = toJSON $ M.toList m

instance FromJSON Hours where
    parseJSON a = Hours . M.fromList <$> parseJSON a

instance PersistField Hours where
  toPersistValue = PersistText . pack . show

  fromPersistValue (PersistText t) = Right . read $ unpack t
  fromPersistValue _ = Left "error deserializing hours"

instance PersistField LocalTime where
  toPersistValue = PersistText . pack . show

  fromPersistValue (PersistText t) = Right . read $ unpack t
  fromPersistValue _ = Left "error deserializing local time"

instance PersistFieldSql Hours where
  sqlType _ = SqlString

instance PersistFieldSql LocalTime where
  sqlType _ = SqlString
