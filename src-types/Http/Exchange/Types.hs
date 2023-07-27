{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}

module Http.Exchange.Types
  ( HttpException(..)
  ) where

import qualified Control.Exception as E

data HttpException
  = ChunkTooLarge
  | ContentLengthMalformed
  | ContentLengthTooLarge
  | ExpectedCrlfAfterChunk
  | ExpectedCrlfAfterChunkLength
  | ExpectedCrlfBeforeChunkLength
  | HeadersMalformed
  | HeadersTooLarge
  | ImplementationMistake
    -- ^ If this one happens, there is a mistake in this
    -- library.
  | NonNumericChunkLength
  | PipelinedResponses
  | UnrecognizedTransferEncoding
  deriving stock (Show)
  deriving anyclass (E.Exception)
