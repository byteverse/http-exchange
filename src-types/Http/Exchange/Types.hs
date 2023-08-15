{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}

module Http.Exchange.Types
  ( HttpException(..)
  ) where

import qualified Control.Exception as E

-- | Exceptions that occur when decoding an HTTP response.
-- If this happens, the only way to proceed is to
-- shut down the connection. Either the server does not
-- speak HTTP correct, or there is a mistake in this libary.
data HttpException
  = ChunkTooLarge
  | ChunkedBodyEndOfInput
  | NonchunkedBodyEndOfInput
  | ContentLengthDuplicated
  | ContentLengthMalformed
  | ContentLengthTooLarge
  | ExpectedCrlfAfterChunk
  | ExpectedCrlfAfterChunkLength
  | ExpectedCrlfBeforeChunkLength
  | HeadersMalformed
  | HeadersEndOfInput
  | HeadersTooLarge
  | ImplementationMistake
    -- ^ If this one happens, there is a mistake in this
    -- library.
  | NonNumericChunkLength
  | PipelinedResponses
  | TransferEncodingUnrecognized
  | TransferEncodingDuplicated
  deriving stock (Show)
  deriving anyclass (E.Exception)
