{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.Json (
    (!)
  , send
  , broadcast
  , receive
  , deriveJson
  , module I
  ) where

import System.IO as I (Handle)
import Control.Applicative ((<$>))
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import Data.Foldable as F (Foldable, mapM_)
import Language.Haskell.TH (Name, Q, Dec)
import System.IO (hFlush)


-- lowercased convenience function
deriveJson :: (String -> String) -> Name -> Q [Dec]
deriveJson = deriveJSON


class CanSendJson h where
  (!) :: ToJSON j => h -> j -> IO ()

instance CanSendJson Handle where
  (!) = send

instance Foldable f => CanSendJson (f Handle) where
  (!) = broadcast


send :: ToJSON j => Handle -> j -> IO ()
send h j = L.hPut h str' >> hFlush h
  where str = encode j 
        str' = runPut $ do
          putWord64le . fromIntegral $ L.length str
          putLazyByteString str

broadcast :: (ToJSON j, Foldable f) => f Handle -> j -> IO ()
broadcast hs j = F.mapM_ (! j) hs


receive :: FromJSON j => Handle -> IO (Maybe j)
receive h = decode <$> do
  len <- fromIntegral . toInteger . runGet getWord64le <$> L.hGet h 8
  L.hGet h len
