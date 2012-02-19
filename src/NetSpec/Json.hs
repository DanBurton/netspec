{-# LANGUAGE FlexibleInstances #-}

-- | Use Lazy 'BSL.ByteString's of JSON to send and receive messages.
-- For this module, a message is prefixed by a
-- 64-bit little-endian signed integer, indicating the length in bytes
-- of the remaining message, which is encoded in JSON format.
module NetSpec.Json (
  -- * Receiving
    receive
  -- * Sending
  , (!)
  , send
  , broadcast
  -- * JSON
  , deriveJson
  , module A
  -- * Other re-exports
  , module I
  , module IO
  ) where

import Data.Aeson as A (ToJSON, FromJSON)
import System.IO as I (Handle)
import Control.Monad.IO.Class as IO (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable as F (Foldable, mapM_)
import Language.Haskell.TH (Name, Q, Dec)

import qualified NetSpec.ByteString as B (send, receive)

-- | Derives 'A.ToJSON' and 'A.FromJSON' instances
-- for your data types. These are necessary in order to
-- use the functions this module provides with your
-- custom data types.
-- 
-- Usage:
-- 
-- @
--  {-\# LANGUAGE TemplateHaskell \#-}
--  data Foo = Bar | Baz { quux :: Int }
--  $(deriveJson id ''Foo)
-- @
-- 
-- Alteratively, you could write your own instances.
deriveJson :: (String -> String) -> Name -> Q [Dec]
deriveJson = deriveJSON


class CanSendJson h where
  -- | The staple for sending a message.
  -- @!@ is typeclassed so that you can
  -- 'send' or 'broadcast' using the same simple syntax.
  -- The @CanSendJson@ typeclass is not exposed.
  -- Instances of CanSendJson include 'I.Handle'
  -- and @Traversable t => t Handle@.
  -- 
  -- @!@ produces an @IO@ action lifted into any 'IO.MonadIO',
  -- so can be used without the extra cruft of 'IO.liftIO'
  -- for most monad stacks. @!@ is declared as @infix 2@.
  -- 
  -- Usage:
  -- 
  -- > destination ! someData
  -- 
  -- Anything that is an instance of 'ToJSON' can be used
  -- on the right-hand side of @!@.
  (!) :: MonadIO io => ToJSON j => h -> j -> io ()

instance CanSendJson Handle where
  (!) = send

instance Foldable f => CanSendJson (f Handle) where
  (!) = broadcast

-- | Send a JSON message to exactly one 'I.Handle'.
send :: MonadIO io => ToJSON j => Handle -> j -> io ()
send h j = liftIO $ B.send h (encode j)

-- | Broadcast a JSON message to multiple 'I.Handle's.
broadcast :: MonadIO io => (ToJSON j, Foldable f) => f Handle -> j -> io ()
broadcast hs j = F.mapM_ (! j) hs


-- | Receive a JSON message from a 'I.Handle'.
-- Unlike 'NetSpec.Text' and 'NetSpec.ByteString', the result
-- of this MonadIO action is wrapped in a 'Maybe'.
-- 'Nothing' means that the data received
-- could not be parsed from JSON to the correct data type.
-- It is up to you to decide whether or not to explicitly handle
-- the 'Nothing' case.
-- 
-- Notice that this action is polymorphic in its return type.
-- Type annotations are usually unnecessary, since type inference
-- can usually determine the correct target type. Example usage:
-- 
-- @
--  do Just m <- receive h
--     case m of Foo x y -> handleFoo x y
--               Bar z   -> handleBar z
-- @
-- 
-- Here @m@ is inferred to have whatever type @Foo@ and @Bar@ belong to.
-- This example code assumes that the JSON parse will succeed.
-- The 'fail' function will be invoked for the Monad you are working
-- in if such a pattern match fails.
receive :: (MonadIO io, FromJSON j) => Handle -> io (Maybe j)
receive h = liftIO $ decode <$> B.receive h
