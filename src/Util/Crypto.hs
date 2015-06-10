module Util.Crypto where

import Control.Monad

import Crypto.BCrypt 

import Data.Text
import Data.Text.Encoding

hashPassword :: Text -> IO (Maybe Text)
hashPassword password = liftM (fmap decodeUtf8) $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8 password

verifyPassword :: Text -> Text -> Bool
verifyPassword hash password = validatePassword (encodeUtf8 hash) (encodeUtf8 password)