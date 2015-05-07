{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import Models
import Config
import Actions.Types
import Util

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Control.Monad.Trans
import Control.Monad.Except

import Data.Time
import Data.Aeson ((.=), object)

registerA :: Action () 
registerA = do
  (request :: RegistrationRequest) <- jsonData
  mHashedPassword <- liftIO . hashPassword $ password request
  case mHashedPassword of
    Nothing -> 
      status internalServerError500

    Just hashedPassword -> do 
      currentTime <- liftIO getCurrentTime
      let user = requestToUser request hashedPassword currentTime
      unlessSucceed conflict409 $ do 
        runDB (DB.insert user)
        status created201


loginA :: Action ()
loginA = do
  (LoginRequest loginEmail loginPassword) <- jsonData
  mUser <- runDB . DB.getBy $ UniqueEmail loginEmail
  case mUser of 
    Nothing ->
      status notFound404

    Just (DB.Entity key user) -> do
      let verified = verifyPassword (userPasswordHash user) loginPassword
      if verified
      then do
        let token = AuthorizationToken key
        tokenText <- encodeToken token
        json (object [ "token" .= tokenText ])
        status ok200

      else
        status unauthorized401

createInviteA :: Action ()
createInviteA = withAuthorization $ \(AuthorizationToken authUserId) -> do
  (CreateInvitationRequest restaurantId time) <- jsonData
  unlessSucceed badRequest400 $ do
    mInvitationId <- runDB (DB.insertUnique (Invitation authUserId restaurantId time DB.Active Nothing))
    case mInvitationId of 
      Nothing -> status conflict409

      Just invitationId -> do
        json (object [ "invitationId" .= invitationId ])
        status created201

