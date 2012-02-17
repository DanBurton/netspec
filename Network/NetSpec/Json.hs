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
import Control.Monad.IO.Class
import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable as F (Foldable, mapM_)
import Language.Haskell.TH (Name, Q, Dec)

import qualified Network.NetSpec.ByteString as B (send, receive)

-- lowercased convenience function
deriveJson :: (String -> String) -> Name -> Q [Dec]
deriveJson = deriveJSON


class CanSendJson h where
  (!) :: MonadIO io => ToJSON j => h -> j -> io ()

instance CanSendJson Handle where
  (!) = send

instance Foldable f => CanSendJson (f Handle) where
  (!) = broadcast


send :: MonadIO io => ToJSON j => Handle -> j -> io ()
send h j = liftIO $ B.send h (encode j)

broadcast :: MonadIO io => (ToJSON j, Foldable f) => f Handle -> j -> io ()
broadcast hs j = F.mapM_ (! j) hs


receive :: (MonadIO io, FromJSON j) => Handle -> io (Maybe j)
receive h = liftIO $ decode <$> B.receive h
