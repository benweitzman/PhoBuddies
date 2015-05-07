{-# LANGUAGE OverloadedStrings #-}

module Actions.Types where

import Models
import Util hiding (decode)

import Control.Monad

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock (UTCTime)
import Data.Time
import Data.Text.Encoding
import qualified Data.Map as M

data RegistrationRequest = RegistrationRequest
  { email :: Text
  , password :: Text
  , age :: Int
  , photoURL :: Text
  , name :: Text
  }

requestToUser :: RegistrationRequest -> Text -> UTCTime -> User
requestToUser RegistrationRequest{ email=email, age=age, photoURL=photoURL, name=name }
              passwordHash = User email passwordHash age photoURL name

instance FromJSON RegistrationRequest where
  parseJSON (Object v) = RegistrationRequest <$>
                         v .: "email" <*>
                         v .: "password" <*>
                         v .: "age" <*>
                         v .: "photo" <*>
                         v .: "name"
  parseJSON _ = mzero                         


data LoginRequest = LoginRequest
  { loginEmail :: Text
  , loginPassword :: Text
  }

instance FromJSON LoginRequest where
    parseJSON (Object v) = LoginRequest <$> v .: "email" <*> v .: "password"
    parseJSON _ = mzero

data CreateInvitationRequest = CreateInvitationRequest
  { restaurantId :: RestaurantId
  , time :: LocalTime
  }

instance FromJSON CreateInvitationRequest where
  parseJSON (Object v) = CreateInvitationRequest <$> v .: "restaurantId" <*> v .: "time"
  parseJSON _ = mzero

data AuthorizationToken = AuthorizationToken { userId :: UserId }

instance Claimable AuthorizationToken where
  toMap (AuthorizationToken userId) = M.fromList [("userId", toJSON userId)]
  fromMap map = do userIdJson <- M.lookup "userId" map
                   userId <- parseMaybe parseJSON userIdJson
                   return (AuthorizationToken userId)