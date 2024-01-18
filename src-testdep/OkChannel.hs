{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

module OkChannel (
  M (..),
  ReceiveException (..),
  SendException,
  showsPrecReceiveException,
  showsPrecSendException,
  Resource,
  send,
  receive,
) where

import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks (ChunksCons, ChunksNil))
import Data.Void (Void, absurd)

import Data.Bytes qualified as Bytes
import Data.Bytes.Chunks qualified as Chunks

type Resource = ()

data ReceiveException = ExpectedMoreInput
  deriving (Show)

showsPrecReceiveException :: Int -> ReceiveException -> String -> String
showsPrecReceiveException = showsPrec

type SendException = Void

showsPrecSendException :: Int -> SendException -> String -> String
showsPrecSendException _ x _ = absurd x

-- First arg is input, second arg is output
-- The input is peeled off one byte sequence at a time by receive
-- We use this feature to feed input byte-by-byte to test streaming
-- features.
data M a = M (Chunks -> Bytes -> (Chunks, Bytes, a))
  deriving stock (Functor)

bindM :: M a -> (a -> M b) -> M b
bindM (M f) g = M $ \inbound0 outbound0 ->
  case f inbound0 outbound0 of
    (inbound1, outbound1, a) ->
      case g a of
        M h -> h inbound1 outbound1

pureM :: a -> M a
pureM a = M $ \x y -> (x, y, a)

instance Applicative M where
  pure = pureM
  f <*> a = f `bindM` \f' -> a `bindM` \a' -> pureM (f' a')

instance Monad M where
  (>>=) = bindM

send ::
  () ->
  Chunks ->
  M (Either SendException ())
send _ b = M $ \inbound outbound ->
  (inbound, outbound <> Chunks.concat b, Right ())

receive ::
  () ->
  M (Either ReceiveException Bytes)
receive _ = M $ \inbound0 outbound ->
  let go inbound = case inbound of
        ChunksNil -> (inbound, outbound, Left ExpectedMoreInput)
        ChunksCons b ch -> case Bytes.null b of
          True -> go ch
          False -> (ch, outbound, Right b)
   in go inbound0
