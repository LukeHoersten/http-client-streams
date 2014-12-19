http-client-streams [![Hackage](https://img.shields.io/hackage/v/http-client-streams.svg?style=flat)](https://hackage.haskell.org/package/http-client-streams) [![Build Status](https://travis-ci.org/dmjio/http-client-streams.svg)](https://travis-ci.org/dmjio/http-client-streams)
===================

## Usage
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified System.IO.Streams as Streams
import           System.IO.Streams.HTTP ( opensslManagerSettings
                                        , parseUrl
                                        , withManager
                                        , withHTTP
                                        , responseBody
                                        , withOpenSSL
                                        , context
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
```

