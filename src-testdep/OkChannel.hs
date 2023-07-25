{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language KindSignatures #-}

module OkChannel
  ( M(..)
  , Exception(..)
  , Resource
  , send
  , receive
  ) where

import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks(ChunksNil,ChunksCons))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks

type Resource = ()

data Exception = ExpectedMoreInput
  deriving (Show)

-- First arg is input, second arg is output
-- The input is peeled off one byte sequence at a time by receive
-- We use this feature to feed input byte-by-byte to test streaming
-- features.
data M a = M (Chunks -> Bytes -> Either Exception (Chunks,Bytes,a))
  deriving stock (Functor)

bindM :: M a -> (a -> M b) -> M b
bindM (M f) g = M $ \inbound0 outbound0 ->
  case f inbound0 outbound0 of
    Left e -> Left e
    Right (inbound1,outbound1,a) ->
      case g a of
        M h -> h inbound1 outbound1

pureM :: a -> M a
pureM a = M $ \x y -> Right (x,y,a)

instance Applicative M where
  pure = pureM
  f <*> a = f `bindM` \f' -> a `bindM` \a' -> pureM (f' a')

instance Monad M where
  (>>=) = bindM

send ::
     ()
  -> Chunks
  -> M ()
send _ b = M $ \inbound outbound ->
  Right (inbound,outbound <> Chunks.concat b,())

receive ::
     ()
  -> M Bytes
receive _ = M $ \inbound0 outbound ->
  let go inbound = case inbound of
        ChunksNil -> Left ExpectedMoreInput
        ChunksCons b ch -> case Bytes.null b of
          True -> go ch
          False -> Right (ch,outbound,b)
   in go inbound0
