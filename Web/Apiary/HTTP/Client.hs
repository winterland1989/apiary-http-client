{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module Web.Apiary.HTTP.Client
    ( HTTPClient
    , initHTTPClient
    , getManager
    , withHTTPClient
    -- ** helpers to make new Request
    , fromWaiRequest
    , fromRequest
    , resetHeaders
    , setPort
    , setHost
    , setHostName
    , setHostHeader
    -- ** send request and get respond
    , sendRequset
    , openRequset
    -- ** send request for side effect
    , sendRequsetNoBody
    -- ** send request and proxy respond
    , proxyTo
    , proxyWith
    , module Network.HTTP.Client
    ) where

import Control.Monad.IO.Class
import Network.HTTP.Client
import qualified Network.Wai as W
import Network.HTTP.Types.Header
import Data.Apiary.Extension
import qualified Data.Proxy.Compat as P (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Default.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Control.Monad.Apiary.Action as A

newtype HTTPClient = HTTPClient Manager

instance Extension HTTPClient

-- |Initialize a @MonadExts@ with @Network.HTTP.Client.ManagerSettings@.
initHTTPClient :: MonadIO m
               => ManagerSettings -> Initializer m exts (HTTPClient ': exts)
initHTTPClient ms = initializer' . liftIO $ newManager ms >>= return . HTTPClient

-- |Get @Network.HTTP.Client.Manage@ from Apiary's @MonadExts@ context.
getManager :: (Has HTTPClient es, MonadExts es m, MonadIO m) => m Manager
getManager = do
    (HTTPClient manager) <- getExt (P.Proxy :: P.Proxy HTTPClient)
    return manager

-- |lift operations with initial manager.
withHTTPClient :: (Has HTTPClient es, MonadExts es m, MonadIO m)
           => (Request -> Manager -> IO a) -> Request -> m a
withHTTPClient f r = do
    manager <- getManager
    liftIO $ f r manager

-- |Copy path, headers, body and queryString from @Network.Wai.Request@
fromWaiRequest
    :: ([T.Text] -> [T.Text])   -- ^ Function to modify request path
    -> ([Header] -> [Header])   -- ^ Function to modify request headers
    -> W.Request -> Request     -- ^ From @Network.Wai.Request@ To @Network.HTTP.Client.Request@
fromWaiRequest pm hm req =
    let
        needsPopper = \ r -> r (W.requestBody req)
        path = pm (W.pathInfo req)
        headers = hm (W.requestHeaders req)
        requestBody' = case W.requestBodyLength req of
            W.ChunkedBody       -> RequestBodyStreamChunked needsPopper
            W.KnownLength len   -> RequestBodyStream (unsafeCoerce len) needsPopper
    in
        def {
            queryString = W.rawQueryString req
        ,   path = T.encodeUtf8 (T.intercalate "/" path)
        ,   requestHeaders = headers
        ,   requestBody = requestBody'
        }

-- |Remove following headers:
-- Transfer-Encoding, Content-Length, Content-Encoding and Accept-Encoding.
-- It's very likely you want to do this.
resetHeaders :: [Header] -> [Header]
resetHeaders = filter (\ (name, _) -> name `notElem` [hTransferEncoding, hContentLength, hContentEncoding, hAcceptEncoding])

-- |Copy path, headers, body and queryString from current @ActionT@'s context.
fromRequest
    :: (Has HTTPClient exts, MonadIO m)
    => ([T.Text] -> [T.Text])   -- ^ Function to modify request path
    -> ([Header] -> [Header])   -- ^ Function to modify request headers
    -> A.ActionT exts prms m Request
fromRequest pm hm= A.getRequest >>= return . fromWaiRequest pm hm

setPort :: Int -> Request -> Request
setPort port req = req{ port = port }

setHostName :: B.ByteString -> Request -> Request
setHostName host req = req{ host = host }

setHostHeader :: B.ByteString -> Request -> Request
setHostHeader host req = req{ requestHeaders = headers }
  where
    oHeaders = requestHeaders req
    headers = (hHost, host) : filter (\ (name, _) -> name /= hHost ) oHeaders

setHost :: B.ByteString -> Request -> Request
setHost host =  setHostHeader host . setHostName host

-- |send requset and get @Response@ @ByteString@
-- For large response consider using @openRequset@ and @responseClose@ instead.
sendRequset :: (Has HTTPClient exts, MonadIO m)
    => Request -> A.ActionT exts prms m (Response LB.ByteString)
sendRequset req = withHTTPClient httpLbs req

-- |send request without receive any body.
sendRequsetNoBody :: (Has HTTPClient exts, MonadIO m)
    => Request -> A.ActionT exts prms m (Response ())
sendRequsetNoBody req = withHTTPClient httpNoBody req

-- |send request and get @Response@ @BodyReader@
openRequset :: (Has HTTPClient exts, MonadIO m)
    => Request -> A.ActionT exts prms m (Response BodyReader)
openRequset req = withHTTPClient responseOpen req

-- |streamming response directly from proxy target.
proxyTo :: (Has HTTPClient exts, MonadIO m)
    => Request -> A.ActionT exts prms m ()
proxyTo req = do
    res <- openRequset req
    A.rawResponse $ \ _ _ ->
        W.responseStream
            (responseStatus res)
            (resetHeaders $ responseHeaders res)
            $ \ sendBuilder flush ->
                let
                    bodyReader = responseBody res
                    loop = do
                        bs <- bodyReader
                        if B.null bs then responseClose res >> flush
                                     else sendBuilder (B.byteString bs) >> loop
                in loop

-- |Modify response from proxy target then send.
-- You should consider remove following headers:
-- Transfer-Encoding, Content-Length, Content-Encoding and Accept-Encoding.
proxyWith
    :: (Has HTTPClient exts, MonadIO m)
    => Request
    -> (Response LB.ByteString -> Response LB.ByteString) -- ^ Function to modify response.
    -> A.ActionT exts prms m ()
proxyWith req modifier = do
    resLbs <- sendRequset req
    let resLbs' = modifier resLbs
    A.status (responseStatus resLbs')
    A.setHeaders (responseHeaders resLbs')
    A.lazyBytes (responseBody resLbs')
