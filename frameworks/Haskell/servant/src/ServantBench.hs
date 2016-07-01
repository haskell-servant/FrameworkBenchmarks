{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ServantBench (run) where

import Data.Aeson hiding (json)
import Data.ByteString.Lazy
import GHC.Exts (IsList(fromList))
import Servant
import qualified Network.Wai.Handler.Warp as Warp

type API =
       "json" :> Get '[JSON] Value
  :<|> "plaintext" :> Get '[PlainText] ByteString

api :: Proxy API
api = Proxy

server :: Server API
server =
      json
 :<|> plaintext

run :: Warp.Port -> IO ()
run p = Warp.run p $ serve api server

instance MimeRender PlainText ByteString where
  mimeRender _ = id
  {-# INLINE mimeRender #-}


-- * Test 1: JSON serialization

json :: Handler Value
json = return . Object $ fromList [("message", "Hello, World!")]
{-# INLINE json #-}

-- * Test 6: Plaintext endpoint

plaintext :: Handler ByteString
plaintext = return "Hello, World!"
{-# INLINE plaintext #-}
