{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Config where

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import Web.Heroku (parseDatabaseUrl)
import Web.Scotty.Trans 
import qualified Web.Scotty.Internal.Types as ST
import Web.JWT

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Except

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Lazy (Text)
import Data.Aeson (Value (Null), (.=), object)
import Data.Default (def)
import Data.Maybe


data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

data Config = Config
  { environment :: Environment
  , jwtSecret :: Secret
  , pool :: DB.ConnectionPool
  }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)


data Failure = Failure Status Text deriving (Show)

unknownFailure :: Failure
unknownFailure = Failure internalServerError500 "Unable to handle request"

instance ScottyError Failure where
    stringError = Failure internalServerError500 . TL.pack
    showError (Failure _ t) = t

type Action a = ActionT Failure ConfigM a

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getPool e
  s <- getJWTSecret
  return Config { environment = e
                , pool = p
                , jwtSecret = s
                }

getEnvironment :: IO Environment
getEnvironment = fmap (maybe Development read) (lookupEnv "SCOTTY_ENV")

getJWTSecret :: IO Secret
getJWTSecret = fmap (secret . T.pack . fromMaybe "this is my jwt secret") (lookupEnv "JWT_SECRET")

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
  case e of
    Development -> runStdoutLoggingT $ DB.createPostgresqlPool s n
    Production -> runStdoutLoggingT $ DB.createPostgresqlPool s n
    Test -> runNoLoggingT $ DB.createPostgresqlPool s n

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e = do
  m <- lookupEnv "DATABASE_URL"
  let s = case m of
        Nothing -> getDefaultConnectionString e
        Just u -> createConnectionString (parseDatabaseUrl u)
  return s

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString e =
  let n = case e of
        Development -> "phobuddies_development"
        Production -> "phobuddies_production"
        Test -> "phobuddies_test"
  in  createConnectionString
        [ ("host", "localhost")
        , ("port", "5432")
        , ("user", "postgres")
        , ("dbname", n)
        ]

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l =
  let f (k, v) = T.concat [k, "=", v]
  in  encodeUtf8 (T.unwords (map f l))

getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1

run :: ScottyT Failure ConfigM () -> Config -> IO ()
run application c = do
  o <- getOptions (environment c)
  let r m = runReaderT (runConfigM m) c
  scottyOptsT o r r application

getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return def
    { settings = s
    , verbose = case e of
      Development -> 1
      Production -> 0
      Test -> 0
    }

getSettings :: Environment -> IO Settings
getSettings e = do
  let s = defaultSettings
      s' = case e of
        Development -> setFdCacheDuration 0 s
        Production -> s
        Test -> s
  m <- getPort
  let s'' = case m of
        Nothing -> s'
        Just p -> setPort p s'
  return s''

getPort :: IO (Maybe Int)
getPort = do
  m <- lookupEnv "PORT"
  let p = case m of
        Nothing -> Nothing
        Just s -> Just (read s)
  return p

runDB :: (MonadTrans t, MonadIO (t ConfigM)) => DB.SqlPersistT IO a -> t ConfigM a
runDB q = do
  p <- lift (asks pool)
  liftIO (DB.runSqlPool q p)

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production = logStdout
loggingM Test = id