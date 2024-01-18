{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Http.Exchange.Types (
  HttpException (..),
) where

import Control.Exception qualified as E
import Data.Bytes (Bytes)

{- | Exceptions that occur when decoding an HTTP response.
If this happens, the only way to proceed is to
shut down the connection. Either the server does not
speak HTTP correct, or there is a mistake in this libary.
-}
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
  | -- | The entire contents of the response.
    HeadersEndOfInput
      {-# UNPACK #-} !Bytes
  | HeadersTooLarge
  | -- | If this one happens, there is a mistake in this
    -- library.
    ImplementationMistake
  | NonNumericChunkLength
  | PipelinedResponses
  | TransferEncodingUnrecognized
  | TransferEncodingDuplicated
  deriving anyclass (E.Exception)

instance Show HttpException where
  showsPrec _ ChunkTooLarge = showString "ChunkTooLarge"
  showsPrec _ ChunkedBodyEndOfInput = showString "ChunkedBodyEndOfInput"
  showsPrec _ NonchunkedBodyEndOfInput = showString "NonchunkedBodyEndOfInput"
  showsPrec _ ContentLengthDuplicated = showString "ContentLengthDuplicated"
  showsPrec _ ContentLengthMalformed = showString "ContentLengthMalformed"
  showsPrec _ ContentLengthTooLarge = showString "ContentLengthTooLarge"
  showsPrec _ ExpectedCrlfAfterChunk = showString "ExpectedCrlfAfterChunk"
  showsPrec _ ExpectedCrlfAfterChunkLength = showString "ExpectedCrlfAfterChunkLength"
  showsPrec _ ExpectedCrlfBeforeChunkLength = showString "ExpectedCrlfBeforeChunkLength"
  showsPrec _ HeadersMalformed = showString "HeadersMalformed"
  showsPrec _ HeadersEndOfInput{} = showString "HeadersEndOfInput{..}"
  showsPrec _ HeadersTooLarge = showString "HeadersTooLarge"
  showsPrec _ ImplementationMistake = showString "ImplementationMistake"
  showsPrec _ NonNumericChunkLength = showString "NonNumericChunkLength"
  showsPrec _ PipelinedResponses = showString "PipelinedResponses"
  showsPrec _ TransferEncodingUnrecognized = showString "TransferEncodingUnrecognized"
  showsPrec _ TransferEncodingDuplicated = showString "TransferEncodingDuplicated"
