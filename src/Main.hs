{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Models
import Config
import Actions.Types
import Actions

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Network.HTTP.Types.Status
import Web.Scotty.Trans
import Data.Aeson (Value (Null), (.=), object)

import Control.Monad.Trans
import Control.Monad.Reader

main :: IO ()
main = do
   c <- getConfig
   run application c

application :: ScottyT Error ConfigM ()
application = do
  runDB (DB.runMigration migrateAll)
  e <- lift (asks environment)
  middleware (loggingM e)
  post "/user" registerA
  get "/authorizationToken" loginA
  post "/invitation" createInviteA
  defaultHandler (defaultH e)

defaultH :: Environment -> Error -> Action ()
defaultH e x = do
  status internalServerError500
  let o = case e of
        Development -> object ["error" .= showError x]
        Production -> Null
        Test -> object ["error" .= showError x]
  json o

