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
                 createUser "kp-7@minio.io"
                 users <- listUsers
                 print users
                 ch <- listChannels
                 print ch

        _ -> print "creds missing"
