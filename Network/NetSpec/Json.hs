{-# LANGUAGE FlexibleInstances #-}

module Network.NetSpec.Json (
    (!)
  , send
  , broadcast
  , receive
  , deriveJson
  , module A
  , module I
  ) where

import Data.Aeson as A (ToJSON, FromJSON)
import System.IO as I (Handle)
import Control.Applicative ((<$>))
import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable as F (Foldable, mapM_)
import Language.Haskell.TH (Name, Q, Dec)

import qualified Network.NetSpec.ByteString as B (send, receive)

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
send h j = B.send h (encode j)

broadcast :: (ToJSON j, Foldable f) => f Handle -> j -> IO ()
broadcast hs j = F.mapM_ (! j) hs


receive :: FromJSON j => Handle -> IO (Maybe j)
receive h = decode <$> B.receive h

