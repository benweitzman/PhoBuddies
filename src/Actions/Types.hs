{-# LANGUAGE OverloadedStrings #-}

module Actions.Types where

import Models
import Util hiding (decode)

import qualified Database.Persist as DB

import Control.Monad
import Control.Applicative

import Data.Text
import Data.Text.Read
import qualified Data.Text.Lazy as LT
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock (UTCTime)
import Data.Time
import Data.Text.Encoding
import qualified Data.Map as M
import Data.Monoid

class FromParams a where
  fromParams :: M.Map Text Text -> Maybe a

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

instance FromParams LoginRequest where
    fromParams m = LoginRequest <$> 
                   M.lookup "email" m <*> 
                   M.lookup "password" m



data LoginResponse = LoginResponse 
  { token :: Text 
  } deriving (Show, Eq)

instance ToJSON LoginResponse where
  toJSON (LoginResponse token) = object [ "token" .= token ]



data CreateInvitationRequest = CreateInvitationRequest
  { restaurantId :: RestaurantId
  , time :: LocalTime
  } deriving (Show, Eq)

instance FromJSON CreateInvitationRequest where
  parseJSON (Object v) = CreateInvitationRequest <$> 
                         v .: "restaurantId" <*> 
                         v .: "time"
  parseJSON _ = mzero



data CreateInvitationResponse = CreateInvitationResponse
  { invitationId :: InvitationId
  } deriving (Show, Eq)

instance ToJSON CreateInvitationResponse where
  toJSON (CreateInvitationResponse inviteId) = object [ "invitationId" .= inviteId ]



data GetInvitationsRequest = GetInvitationsRequest
  { offset :: Maybe Int
  } deriving (Show, Eq)

instance FromParams GetInvitationsRequest where
  fromParams paramMap = GetInvitationsRequest <$>
                          case M.lookup "offset" paramMap of
                            Nothing -> pure Nothing

                            Just text -> Just <$> textToInt text

    where textToInt t = let eitherPair = decimal t
                            maybePair = eitherToMaybe eitherPair
                        in fst <$> maybePair



newtype InvitationWithId = InvitationWithId (DB.Entity Invitation)

instance ToJSON InvitationWithId where
  toJSON (InvitationWithId (DB.Entity key record)) = object ["id" .= key, "invitation" .= record]

data GetInvitationsResponse = GetInvitationsResponse
  { invitations :: [InvitationWithId]
  , nextOffset :: Maybe Int
  } 



instance ToJSON GetInvitationsResponse where
  toJSON (GetInvitationsResponse invitations offset) = object ["invitations" .= invitations, "nextOffset" .= offset]

data AuthorizationToken = AuthorizationToken 
  { userId :: UserId 
  } deriving (Show, Eq)

instance Claimable AuthorizationToken where
  toMap (AuthorizationToken userId) = M.fromList [("userId", toJSON userId)]
  fromMap map = do userIdJson <- M.lookup "userId" map
                   userId <- parseMaybe parseJSON userIdJson
                   return (AuthorizationToken userId)