> {-# LANGUAGE RankNTypes #-}

> module Control.Quiver.Wai (
>   qapp,
> ) where

> import Control.Monad
> import Control.Quiver
> import Control.Quiver.SP
> import Data.ByteString (ByteString)
> import Data.ByteString.Builder
> import Network.HTTP.Types as HTTP
> import Network.Wai as Wai

> import qualified Data.ByteString as ByteString
> import qualified Network.Wai as Wai

> qapp :: (Wai.Request -> forall b b' . P () Builder b b' IO (HTTP.Status, HTTP.ResponseHeaders, P () Builder Builder () IO (SPResult ()))) -> Wai.Application
> qapp handler req sendResponse = do
>   (src', (status, headers, handler')) <- sprun $ src +>->> handler req'
>   sendResponse $ Wai.responseStream status headers $ \ sendChunk flushChunks ->
>     let sink = consume () (\c -> qlift (sendChunk c) >> sink) (qlift flushChunks)
>      in void $ sprun $ src' >->> handler' >->> sink
>  where
>   src = qlift (requestBody req) >>= \chunk -> if ByteString.null chunk then spcomplete else byteString chunk >:> src
>   req' = req { requestBody = error "Wai.Request.requestBody must not be accessed directly in a Quiver application" }

