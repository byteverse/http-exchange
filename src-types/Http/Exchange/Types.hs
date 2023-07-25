{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}

module Http.Exchange.Types
  ( Exception(..)
  ) where

import qualified Control.Exception as E

data Exception
  = UnrecognizedTransferEncoding
  | ContentLengthMalformed
  | ContentLengthTooLarge
  | HeadersTooLarge
  | HeadersMalformed
  | PipelinedResponses
  | ExpectedMoreInput
  | ImplementationMistake
  | ChunkTooLarge
  | ExpectedCrlfAfterChunkLength
  | ExpectedCrlfBeforeChunkLength
  | ExpectedCrlfAfterChunk
  | NonNumericChunkLength
  deriving stock (Show)
  deriving anyclass (E.Exception)
