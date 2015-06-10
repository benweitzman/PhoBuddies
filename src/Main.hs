{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Models
import Config
import Actions.Types
import Actions

import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Web.Scotty.Trans
import Data.Aeson (Value (Null), (.=), object)

import Control.Monad.Trans
import Control.Monad.Reader

main :: IO ()
main = do
   c <- getConfig
   run application c

application :: ScottyT Failure ConfigM ()
application = do
  runDB (DB.runMigration migrateAll)
  e <- lift (asks environment)
  middleware $ loggingM e
  post "/user" $ withJSON registerA
  get "/authorizationToken" . void $ withParams loginA
  post "/invitation" . void . withJSON $ withAuthorization  createInviteA
  get "/invitation" . void $ withParams getInvitesA
  defaultHandler $ defaultH e

defaultH :: Environment -> Failure -> Action ()
defaultH e (Failure stat message) = do
  status  stat
  let o = case e of
        Development -> object ["error" .= message]
        Production -> Null
        Test -> object ["error" .= message]
  json o