Apiary HTTP Client
==================

[![Hackage](https://img.shields.io/hackage/v/apiary-http-client.svg?style=flat-square)](http://hackage.haskell.org/package/apiary-http-client)

A HTTP Client for [Apiary](http://hackage.haskell.org/package/apiary), using `Apiary`'s extension api, suitable for proxying HTTP request to backend API, with flexible APIs and streamming proxying abilities.

This module also reexport `Network.HTTP.Client`. 

Example
-------

```haskell
import Web.Apiary
import Network.Wai.Handler.Warp
import Web.Apiary.HTTP.Client as HTTP

main :: IO ()
main = runApiaryWith (run serverPort) (HTTP.initHTTPClient HTTP.defaultManagerSettings) def $ do

    [capture|/query|] . action $ do
        
        -- make a new Network.HTTP.Client.Request from current ActionT's Network.Wai.Request
        -- it's recommended to use resetHeaders to remove following headers:
        -- Transfer-Encoding, Content-Length, Content-Encoding and Accept-Encoding.
        req <- HTTP.fromRequest id resetHeaders

        -- set proxying host and port
        -- use function from Network.HTTP.Client to modify more
        let req' = HTTP.setHost "api.backend.com" . HTTP.setPort 80 $ req

        -- send request and proxy respond in streamming fashion.
        HTTP.proxyTo req'
```
