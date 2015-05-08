{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Aeson ((.=), object, FromJSON, ToJSON)
import Data.Maybe
import qualified Data.Map as M

type API request response = Action response

api :: (ToJSON response) => Action (Status, response) -> API request response
api with = do 
  (stat, resp) <- with
  json resp
  status stat
  return resp

withJSON :: (FromJSON request) => (request -> Action response) -> API request response
withJSON with = do
    req <- jsonData
    with req

withParams :: (FromParams request) => (request -> Action response) -> API request response
withParams with = do 
  ps <- params
  case fromParams (M.fromList ps) of
    Nothing -> raise $ Failure badRequest400 "Expected request in params"

    Just req -> with req


registerA :: API RegistrationRequest ()
registerA = api $ 
            withJSON $ \request -> do
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
loginA = api $ 
         withJSON $ \(LoginRequest loginEmail loginPassword) -> do
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

createInviteA :: API CreateInvitationRequest CreateInvitationResponse
createInviteA = api $ 
                withJSON $ \(CreateInvitationRequest restaurantId time) ->
                withAuthorization $ \(AuthorizationToken authUserId) -> do 
  mInvitationId <- runDB (DB.insertUnique (Invitation authUserId restaurantId time DB.Active Nothing))
  case mInvitationId of 
    Nothing -> 
      raise $ Failure conflict409 "One invitation exists"

    Just invitationId ->
      return (created201, CreateInvitationResponse invitationId)    

getInvitesA :: API GetInvitationsRequest GetInvitationsResponse
getInvitesA = api $
              withParams $ \(GetInvitationsRequest offset) -> do
  invitations <- runDB (DB.selectList filter [DB.OffsetBy (fromMaybe 0 offset)
                                             ,DB.LimitTo limit
                                             ])
  count <- runDB $ DB.count filter 
  let nextOffset = maybe limit (+ limit) offset 
      mNextOffset = if nextOffset >= count then Nothing else Just nextOffset
  return (ok200, GetInvitationsResponse (InvitationWithId <$> invitations) mNextOffset)
  where limit = 100
        filter = [InvitationCurrent ==. DB.Active]

