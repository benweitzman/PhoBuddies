{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import Models
import Config
import Actions.Types
import Util

import Database.Persist ((==.))
import qualified Database.Persist as DB 
import qualified Database.Persist.Postgresql as DB
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Control.Monad.Trans
import Control.Monad.Except

import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT 
import Data.Aeson ((.=), object, FromJSON, ToJSON)
import Data.Maybe
import qualified Data.Map as M

type API request response = request -> Action (Status, response)

type Authorized auth request response = auth -> API request response

api :: (ToJSON response) => Action (Status, response) -> Action response
api with = do 
  (stat, resp) <- with
  json resp
  status stat
  return resp

withJSON :: (FromJSON request, ToJSON response) => API request response -> Action response
withJSON with = do
    req <- jsonData
    api $ with req

withParams :: (FromParams request, ToJSON response) => API request response -> Action response
withParams with = do 
  paramAssoc <- M.fromList <$> params
  let ps = M.mapKeys LT.toStrict $ LT.toStrict <$> paramAssoc
  case fromParams ps of
    Nothing -> raise $ Failure badRequest400 "Expected request in params"

    Just req -> api $ with req

withAuthorization :: Claimable a => Authorized a request response -> API request response
withAuthorization with req = do 
    mAuthTokenText <- requestHeader "Authorization"
    case mAuthTokenText of 
        Nothing -> raise $ Failure unauthorized401 "Authorization required"

        Just authTokenText -> do
            mAuthToken <- decodeToken authTokenText
            case mAuthToken of
                Nothing -> raise $ Failure unauthorized401 "Invalid authorization"

                Just authToken -> with authToken req


registerA :: API RegistrationRequest ()
registerA request = do
  mHashedPassword <- liftIO . hashPassword $ password request
  case mHashedPassword of
    Nothing -> 
      raise unknownFailure

    Just hashedPassword -> do 
      currentTime <- liftIO getCurrentTime
      let user = requestToUser request hashedPassword currentTime
      mKey <- runDB (DB.insertUnique user)
      case mKey of 
        Nothing -> 
          raise $ Failure conflict409 "Duplicate email"

        Just _ -> 
          return (created201, ())


loginA :: API LoginRequest LoginResponse
loginA (LoginRequest loginEmail loginPassword) = do
  mUser <- runDB . DB.getBy $ UniqueEmail loginEmail
  case mUser of 
    Nothing ->
      raise $ Failure notFound404 "Unknown email"

    Just (DB.Entity key user) -> do
      let verified = verifyPassword (userPasswordHash user) loginPassword
      if verified
      then do
        let token = AuthorizationToken key
        tokenText <- encodeToken token
        return (ok200, LoginResponse tokenText)

      else
        raise $ Failure unauthorized401 "Bad password"

createInviteA :: Authorized AuthorizationToken CreateInvitationRequest CreateInvitationResponse
createInviteA (AuthorizationToken authUserId) (CreateInvitationRequest restaurantId time) = do 
    mInvitationId <- runDB (DB.insertUnique (Invitation authUserId restaurantId time DB.Active Nothing))
    case mInvitationId of 
      Nothing -> 
        raise $ Failure conflict409 "One invitation exists"

      Just invitationId ->
        return (created201, CreateInvitationResponse invitationId)    

getInvitesA :: API GetInvitationsRequest GetInvitationsResponse
getInvitesA (GetInvitationsRequest offset) = do
  invitations <- runDB (DB.selectList filter [DB.OffsetBy (fromMaybe 0 offset)
                                             ,DB.LimitTo limit
                                             ])
  count <- runDB $ DB.count filter 
  let nextOffset = maybe limit (+ limit) offset 
      mNextOffset = if nextOffset >= count then Nothing else Just nextOffset
  return (ok200, GetInvitationsResponse (InvitationWithId <$> invitations) mNextOffset)
  where limit = 100
        filter = [InvitationCurrent ==. DB.Active]

