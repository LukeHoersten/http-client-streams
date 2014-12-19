{-# LANGUAGE OverloadedStrings #-}
module Main where

import           OpenSSL                ( withOpenSSL )
import           OpenSSL.Session        ( context )
import qualified System.IO.Streams as Streams
import           System.IO.Streams.HTTP ( opensslManagerSettings
                                        , parseUrl
                                        , withManager
                                        , withHTTP
                                        , responseBody
                                        )

------------------------------------------------------------------------------
-- | OpenSSL test
main :: IO ()
main = withOpenSSL $ do
  let settings = opensslManagerSettings context         
  req <- parseUrl "https://google.com"
  withManager settings $ \mgr ->
    withHTTP req mgr $ \resp ->
      Streams.supplyTo Streams.stdout (responseBody resp)
  
