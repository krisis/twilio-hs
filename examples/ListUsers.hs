{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ListUsers where

import           Twilio.API
import           Twilio.Types

import           Protolude

main = do
    credsM <- loadTwilioCreds
    case credsM of
        Just creds -> void $ runTwilioM creds $ do
                 createUser "someone@nowhere.org"
                 users <- listUsers
                 print users
                 createChannel "general"
                 ch <- listChannels
                 print ch

        _ -> print "creds missing"
