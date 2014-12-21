-- | Here is an example GET request that streams the response body to standard
--   output:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >  
-- > import qualified System.IO.Streams as Streams
-- > import           System.IO.Streams.HTTP ( opensslManagerSettings
-- >                                         , parseUrl
-- >                                         , withManager
-- >                                         , withHTTP
-- >                                         , responseBody
-- >                                         , withOpenSSL
-- >                                         , context
-- >                                         , requestBody
-- >                                         , stream
-- >                                         , method
-- >                                         )
-- >  
-- > ------------------------------------------------------------------------------
-- > -- | OpenSSL test
-- > main :: IO ()
-- > main = withOpenSSL $ do
-- >   let settings = opensslManagerSettings context
-- >   req <- parseUrl "https://google.com"
-- >   withManager settings $ \mgr ->
-- >     withHTTP req mgr $ \resp ->
-- >       Streams.supplyTo Streams.stdout (responseBody resp)
-- >
-- > ------------------------------------------------------------------------------
-- > -- | POST test
-- > post :: IO ()
-- > post = withOpenSSL $ do
-- >   let settings = opensslManagerSettings context
-- >   req <- parseUrl "https://google.com"
-- >   let request = req { method = "POST"
-- >                     , requestBody = stream $ Streams.fromLazyByteString "body"
-- >                     }
-- >   withManager settings $ \mgr ->
-- >     withHTTP req mgr $ \resp ->
-- >       Streams.supplyTo Streams.stdout (responseBody resp)

--
-- For non-streaming request bodies, study the 'RequestBody' type,
-- which also
-- accepts strict \/ lazy bytestrings

module System.IO.Streams.HTTP (
    -- * http-client
    -- $httpclient
    module Network.HTTP.Client
  , module Network.HTTP.Client.OpenSSL
  , module OpenSSL
  , module OpenSSL.Session
    -- * io-streams Interface
  , withHTTP
  , streamN
  , stream
  ) where

import           Control.Applicative     ( (<$>) )
import           Control.Monad.IO.Class  ( liftIO )
import           Data.ByteString         ( ByteString )
import qualified Data.ByteString as B
import           Data.Int                ( Int64 )
import           Network.HTTP.Client
import           Network.HTTP.Client.OpenSSL

import           OpenSSL
import           OpenSSL.Session

import           System.IO               ( stdout )
import           System.IO.Streams       ( InputStream
                                         , OutputStream
                                         , Generator
                                         )

import qualified System.IO.Streams as Streams
import           System.IO.Streams.ByteString

{- $httpclient
    This module is a thin @io-streams@ wrapper around the @http-client@ and
    @http-client-openssl@ libraries.

    If you'd rather use the `tls` library for encryption please see this package: <https://hackage.haskell.org/package/io-streams-http>

    Read the documentation in the "Network.HTTP.Client" module of the
    @http-client@ library to learn about how to:

    * manage connections using connection pooling,

    * use more advanced request\/response features,

    * handle exceptions, and:

    * manage cookies.

    @http-client-openssl@ provides support for TLS connections (i.e.
     HTTPS).
-}

------------------------------------------------------------------------------
-- | Send an HTTP 'Request' and wait for an HTTP 'Response'
withHTTP
    :: Request
    -> Manager
    -> (Response (InputStream ByteString) -> IO a)
    -> IO a
withHTTP r m k = withResponse r m k'
  where
    k' resp = do
      p <- (from . brRead . responseBody) resp
      k (resp { responseBody = p})
{-# INLINABLE withHTTP #-}

------------------------------------------------------------------------------
-- | Produce an InputStream from a streaming IO ByteString action
from :: IO ByteString -> IO (InputStream ByteString)
from io = Streams.makeInputStream $ do
            bs <- io
            return $ if B.null bs
              then Nothing
              else Just bs

------------------------------------------------------------------------------
-- | Stream body of request
to :: IO (InputStream ByteString) -> (IO ByteString -> IO ()) -> IO ()
to m f = do
  is <- m
  f $ maybe B.empty id <$> Streams.read is

------------------------------------------------------------------------------
-- | Stream body of request
stream :: IO (InputStream ByteString) -> RequestBody
stream p = RequestBodyStreamChunked (to p)
{-# INLINABLE stream #-}

------------------------------------------------------------------------------
-- | Stream with N bytes exactly
streamN :: Int64 -> IO (InputStream ByteString) -> RequestBody
streamN n p = RequestBodyStream n (to p)
{-# INLINABLE streamN #-}

