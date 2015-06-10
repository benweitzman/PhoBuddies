{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( eitherToMaybe
  , requestHeader
  , module Util.Crypto
  , module Util.JWT
  ) where

import Config

import Control.Monad

import Network.HTTP.Types.Status
import Network.Wai
import Web.Scotty.Trans

import Data.Text
import Data.Text.Encoding
import Data.CaseInsensitive 

import Util.Crypto
import Util.JWT

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val


requestHeader :: Text -> Action (Maybe Text)
requestHeader headerName = do 
  headers <- requestHeaders <$> request 
  let headerNameCI = mk $ encodeUtf8 headerName
  return $ decodeUtf8 <$> lookup headerNameCI headers



                            