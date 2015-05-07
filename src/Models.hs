{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Models.Hours
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time (TimeOfDay, LocalTime)
import Database.Persist.Sql
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
  sqlSettings)
import Data.Aeson

instance ToJSON Checkmark where
  toJSON Inactive = toJSON False
  toJSON Active = toJSON True

instance FromJSON Checkmark where
  parseJSON x = fromBool <$> parseJSON x
    where fromBool True = Active
          fromBool False = Inactive

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
User
  email Text
  passwordHash Text
  age Int
  photoURL Text
  name Text
  created UTCTime default=now()
  UniqueEmail email

Restaurant json
  name Text
  description Text
  address Text
  latitude Double
  longitude Double
  photoURL Text
  hours Hours
  created UTCTime default=now()

Invitation json
  host UserId
  location RestaurantId
  time LocalTime
  current Checkmark nullable
  guest UserId Maybe default=NULL
  UniqueInvitation host current !force


InvitationResponse json
  invitation InvitationId
  guest UserId
  current Checkmark nullable
  UniqueResponse invitation current !force
|]
