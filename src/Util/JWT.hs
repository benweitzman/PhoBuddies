module Util.JWT where 

import Config

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative

import Web.JWT 

import Data.Aeson (Value)
import Data.Map
import Data.Text
import Data.Text.Encoding
import Data.CaseInsensitive 

jwtAlgorithm :: Algorithm
jwtAlgorithm = HS256

class Claimable a where
  toMap :: a -> Map Text Value 
  fromMap :: Map Text Value -> Maybe a 

encodeToken :: (Claimable a, MonadTrans t, Monad (t ConfigM)) => a -> t ConfigM Text
encodeToken claims = do 
  secret <- lift $ asks jwtSecret 
  return $ encodeSigned jwtAlgorithm secret cs 
  where cs = def { unregisteredClaims = toMap claims }

decodeToken :: (Claimable a, MonadTrans t, Monad (t ConfigM)) => Text -> t ConfigM (Maybe a)
decodeToken json = do 
  secret <- lift $ asks jwtSecret
  return $ case decode json of
    Nothing -> Nothing

    -- prevent the client from modifying the jwt and changing the algorithm to "none", 
    -- which is valid with any signature. 
    Just jwt -> do 
      givenAlgo <- alg $ header jwt
      unless (givenAlgo == jwtAlgorithm) Nothing
      verify secret jwt >>= fromMap . unregisteredClaims . claims