{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import Config

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Network.HTTP.Types.Status
import Network.Wai
import Web.Scotty.Trans
import Web.JWT 

import Data.Aeson
import Data.Map
import Data.Text
import Data.Text.Encoding
import Data.CaseInsensitive 

import Crypto.BCrypt 


hashPassword :: Text -> IO (Maybe Text)
hashPassword password = liftM (fmap decodeUtf8) $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8 password

verifyPassword :: Text -> Text -> Bool
verifyPassword hash password = validatePassword (encodeUtf8 hash) (encodeUtf8 password)


{-
unlessSucceed :: (Monad m, ScottyError e) => Status -> ActionT e m () -> ActionT e m ()
unlessSucceed st block = catchError block (\_ -> status st)
-}

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val



class Claimable a where
    toMap :: a -> Map Text Value 
    fromMap :: Map Text Value -> Maybe a 

encodeToken :: (Claimable a, MonadTrans t, Monad (t ConfigM)) => a -> t ConfigM Text
encodeToken claims = do 
    secret <- lift $ asks jwtSecret 
    return $ encodeSigned HS256 secret cs 
    where cs = def { unregisteredClaims = toMap claims }

decodeToken :: (Claimable a, MonadTrans t, Monad (t ConfigM)) => Text -> t ConfigM (Maybe a)
decodeToken json = do 
    secret <- lift $ asks jwtSecret 
    return $ decodeAndVerifySignature secret json >>= fromMap . unregisteredClaims . claims





requestHeader :: Text -> Action (Maybe Text)
requestHeader headerName = do 
    headers <- requestHeaders <$> request 
    let headerNameCI = mk $ encodeUtf8 headerName
    return $ decodeUtf8 <$> Prelude.lookup headerNameCI headers

withAuthorization :: Claimable a => (a -> Action b) -> Action b
withAuthorization with = do 
    mAuthTokenText <- requestHeader "Authorization"
    case mAuthTokenText of 
        Nothing -> raise $ Failure unauthorized401 "Authorization required"

        Just authTokenText -> do
            mAuthToken <- decodeToken authTokenText
            case mAuthToken of
                Nothing -> raise $ Failure unauthorized401 "Invalid authorization"

                Just authToken -> with authToken

                            