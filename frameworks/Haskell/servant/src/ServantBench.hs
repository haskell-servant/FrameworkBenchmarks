{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module ServantBench (run) where

import           Control.Exception        (bracket)
import           Control.Monad            (replicateM)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               hiding (json)
import qualified Data.ByteString          as BS
import           Data.ByteString.Lazy
import           Data.Int                 (Int32)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import qualified Data.Text                as Text
import           GHC.Exts                 (IsList (fromList))
import           GHC.Generics             (Generic)
import qualified Hasql.Decoders           as HasqlDec
import qualified Hasql.Encoders           as HasqlEnc
import           Hasql.Pool               (Pool, acquire, release, use)
import qualified Hasql.Query              as Hasql
import           Hasql.Session            (query)
import           Lucid
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.HTML.Lucid       (HTML)
import           System.Random.MWC        (GenIO, createSystemRandom, uniformR)

type API =
       "json" :> Get '[JSON] Value
  :<|> "db" :> Get '[JSON] World
  :<|> "queries" :> QueryParam "queries" Int :> Get '[JSON] [World]
  :<|> "fortune" :> Get '[HTML] (Html ())
  :<|> "plaintext" :> Get '[PlainText] ByteString

api :: Proxy API
api = Proxy

server :: Pool -> GenIO -> Server API
server pool gen =
      json
 :<|> singleDb pool gen
 :<|> multipleDb pool gen
 :<|> fortunes pool
 :<|> plaintext

run :: Warp.Port -> BS.ByteString -> IO ()
run port dbSettings = do
  gen <- createSystemRandom
  bracket (acquire settings) release $ \pool ->
    Warp.run port $ serve api $ server pool gen
  where
    halfSecond = 0.5
    settings = (30, halfSecond, dbSettings)

instance MimeRender PlainText ByteString where
  mimeRender _ = id
  {-# INLINE mimeRender #-}

data World = World { wId :: !Int32 , wRandomNumber :: !Int32 }
  deriving (Show, Generic)

instance ToJSON World where
  toEncoding w =
    pairs (  "id"           .= wId w
          <> "randomNumber" .= wRandomNumber w
          )

data Fortune = Fortune { fId :: !Int32 , fMessage :: Text.Text }
  deriving (Show, Generic)

instance ToJSON Fortune where
  toEncoding f =
    pairs (  "id"      .= fId f
          <> "message" .= fMessage f
          )


------------------------------------------------------------------------------

-- * Test 1: JSON serialization

json :: Handler Value
json = return . Object $ fromList [("message", "Hello, World!")]
{-# INLINE json #-}


-- * Test 2: Single database query

selectSingle :: Hasql.Query Int32 World
selectSingle = Hasql.statement q encoder decoder True
  where
   q = "SELECT * FROM World WHERE (id = $1)"
   encoder = HasqlEnc.value HasqlEnc.int4
   decoder = HasqlDec.singleRow $ World <$> HasqlDec.value HasqlDec.int4
                                        <*> HasqlDec.value HasqlDec.int4
{-# INLINE selectSingle #-}

singleDb :: Pool -> GenIO -> Handler World
singleDb pool gen = do
  v <- liftIO $ uniformR (1, 10000) gen
  r <- liftIO $ use pool (query v selectSingle)
  case r of
    Left e -> throwError err500
    Right world -> return world
{-# INLINE singleDb #-}


-- * Test 3: Multiple database query

multipleDb :: Pool -> GenIO -> Maybe Int -> Handler [World]
multipleDb pool gen mcount = replicateM count $ singleDb pool gen
  where
    count = let c = fromMaybe 1 mcount in max 1 (min c 500)
{-# INLINE multipleDb #-}


-- * Test 4: Fortunes

selectFortunes :: Hasql.Query () [Fortune]
selectFortunes = Hasql.statement q encoder decoder True
  where
   q = "SELECT * FROM Fortune"
   encoder = HasqlEnc.unit
   -- TODO: investigate whether 'rowsList' is worth the more expensive 'cons'.
   decoder = HasqlDec.rowsList $ Fortune <$> HasqlDec.value HasqlDec.int4
                                         <*> HasqlDec.value HasqlDec.text
{-# INLINE selectFortunes #-}

fortunes :: Pool -> Handler (Html ())
fortunes pool = do
  r <- liftIO $ use pool (query () selectFortunes)
  case r of
    Left e -> throwError err500
    Right fs -> return $ do
      html_ $ do
        body_$ do
          table_ $ do
            tr_ $ do
              th_ "id"
              th_ "message"
            mapM_ (\f -> tr_ $ do
              td_ (p_ . toHtml . show $ fId f)
              td_ (p_ . toHtml $ fMessage f)) fs
{-# INLINE fortunes #-}


-- * Test 6: Plaintext endpoint

plaintext :: Handler ByteString
plaintext = return "Hello, World!"
{-# INLINE plaintext #-}
