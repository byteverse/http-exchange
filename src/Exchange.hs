{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Exchange
  ( Exception (..)
  , HttpException (..)
  , exchange
  , exchangeDiscardBody
  ) where

import Channel (M, ReceiveException, Resource, SendException, receive, send)
import Control.Monad (when)
import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks (ChunksCons, ChunksNil))
import Data.Bytes.Parser (Parser)
import Data.Char (ord)
import Data.Word (Word64)
import Http.Bodied (Bodied (Bodied))
import Http.Exchange.Types (HttpException)
import Http.Header (Header (Header))
import Http.Types (Headers, LookupException (Duplicate, Missing), Request, Response)
import Text.Read (readMaybe)

import Channel qualified
import Control.Exception qualified
import Data.Bytes qualified as Bytes
import Data.Bytes.Chunks qualified as Chunks
import Data.Bytes.Parser qualified as Parser
import Data.Bytes.Parser.Latin qualified as Latin
import Data.Text qualified as T
import Http.Bodied qualified
import Http.Exchange.Types qualified as E
import Http.Header qualified
import Http.Headers qualified as Headers
import Http.Request qualified as Request
import Http.Response qualified as Response

data Continuation
  = Continuation
      !Instruction
      !Chunks -- these chunks are in reverse order

-- Not exported
data Instruction
  = More -- we are in the middle of a chunk
      !Int -- how much input was requested (zero is special)
      !Int -- how much more input do we need to consume
  | MorePostCr
      !Int -- how much input was requested for the last chunk
  | ChunkLength
      -- We are in the middle (or at the beginning) of chunk length,
      -- the leading CRLF has already been consumed
      !Word64 -- chunk length accumulator
  | PostCr -- We already got the CR after the chunk length
      !Int -- how much input we need to consume, but we need to consume the LF first
  | -- | We got all the chunks, and we got the zero-length chunk
    -- at the end, and we got the trailing CRLF. We are done.
    Done

data TransferEncoding
  = Nonchunked
  | Chunked

-- | An exception that occurs during an HTTP exchange.
data Exception
  = -- | The response was not a valid HTTP response
    Http
      !HttpException
  | -- | Transport exception while sending. When backed by stream sockets,
    -- exceptions like @ECONNRESET@ show up here.
    Send
      !SendException
  | -- | Transport exception while receiving. Depending on the backend,
    -- this may or may not include an end-of-input exception. For stream
    -- sockets, end-of-input is not presented as an exception. It is
    -- presented as a zero-length result.
    Receive
      !ReceiveException
  deriving anyclass (Control.Exception.Exception)

instance Show Exception where
  showsPrec d (Http e) =
    showParen
      (d > 10)
      (showString "Http " . showsPrec 11 e)
  showsPrec d (Send e) =
    showParen
      (d > 10)
      (showString "Send " . Channel.showsPrecSendException 11 e)
  showsPrec d (Receive e) =
    showParen
      (d > 10)
      (showString "Receive " . Channel.showsPrecReceiveException 11 e)

{- | Variant of @exchange@ that discards the response body. This can be
used safely even when the size of the response body is greater than
the amount of memory available.

This is intended as a resident-memory optimization for situations where
the caller ignores the response body.
-}
exchangeDiscardBody ::
  Resource ->
  Bodied Request -> -- http request line and headers
  M (Either Exception Response)
exchangeDiscardBody ctx req = do
  let enc = Request.bodiedToChunks req
  send ctx enc >>= \case
    Left err -> pure (Left (Send err))
    Right () -> receiveResponseDiscardBody ctx

{- | Send an HTTP request and await a response. This function takes
responsibility for encoding the request and decoding the response.
It deals with the @Transfer-Encoding@ of the response and supports
both chunked and nonchunked responses.
-}
exchange ::
  Resource ->
  Bodied Request -> -- http request line and headers
  M (Either Exception (Bodied Response))
exchange ctx req = do
  let enc = Request.bodiedToChunks req
  send ctx enc >>= \case
    Left err -> pure (Left (Send err))
    Right () -> receiveResponsePreserveBody ctx

-- Returns response. Also returns leftovers that belong to the body.
receiveHeaders ::
  Resource ->
  M (Either Exception (Response, Bytes))
receiveHeaders !ctx = go mempty
 where
  go :: Bytes -> M (Either Exception (Response, Bytes))
  go !oldOutput =
    receive ctx >>= \case
      Left err -> pure (Left (Receive err))
      Right newOutput -> case Bytes.length newOutput of
        0 -> pure (Left (Http (E.HeadersEndOfInput oldOutput)))
        _ -> do
          let output = oldOutput <> newOutput
          case splitEndOfHeaders output of
            Nothing ->
              if Bytes.length output > 16000
                then pure (Left (Http E.HeadersTooLarge))
                else go output
            Just (pre, post) -> case Response.decode 128 pre of
              Nothing -> pure (Left (Http E.HeadersMalformed))
              Just resp -> pure (Right (resp, post))

receiveResponsePreserveBody ::
  Resource ->
  M (Either Exception (Bodied Response))
receiveResponsePreserveBody !ctx =
  receiveHeaders ctx >>= \case
    Left err -> pure (Left err)
    Right (resp@Response.Response {headers}, post) -> case lookupTransferEncoding headers of
      Left err -> pure (Left (Http err))
      Right enc -> case enc of
        Nonchunked -> handleNonchunkedBody ctx resp post headers
        Chunked -> handleChunkedBody ctx resp post

receiveResponseDiscardBody ::
  Resource ->
  M (Either Exception Response)
receiveResponseDiscardBody !ctx =
  receiveHeaders ctx >>= \case
    Left err -> pure (Left err)
    Right (resp@Response.Response {headers}, post) -> case lookupTransferEncoding headers of
      Left err -> pure (Left (Http err))
      Right enc -> case enc of
        Nonchunked -> discardNonchunkedBody ctx resp post headers
        Chunked -> discardChunkedBody ctx resp post

handleChunkedBody ::
  Resource ->
  Response ->
  Bytes ->
  M (Either Exception (Bodied Response))
handleChunkedBody !ctx resp !input0 = do
  let go contA !inputA = case Parser.parseBytes (parserChunked contA) inputA of
        Parser.Failure e -> pure (Left (Http e))
        Parser.Success (Parser.Slice _ leftoverLen contB) -> case leftoverLen of
          -- We expect that parserChunked consumes all input, so we check
          -- here to be certain that it actually does.
          0 -> case contB of
            Continuation Done revChunks ->
              pure $
                Right $
                  Bodied
                    { metadata = resp
                    , body = Chunks.reverse revChunks
                    }
            _ ->
              receive ctx >>= \case
                Right inputB -> case Bytes.length inputB of
                  0 -> pure (Left (Http E.ChunkedBodyEndOfInput))
                  _ -> go contB inputB
                Left err -> pure (Left (Receive err))
          _ -> pure (Left (Http E.ImplementationMistake))
  let cont0 = Continuation (ChunkLength 0) ChunksNil
  go cont0 input0

parserChunked :: Continuation -> Parser HttpException s Continuation
parserChunked (Continuation instr chunks0) = case instr of
  Done -> Parser.fail E.ImplementationMistake
  More total n -> parserChunkedMore total n chunks0
  MorePostCr total -> parserChunkedMorePostCr total chunks0
  ChunkLength acc -> parserChunkedChunkLength acc chunks0
  PostCr n -> parserChunkedChunkLengthPostCr n chunks0

parserChunkedMore :: Int -> Int -> Chunks -> Parser HttpException s Continuation
parserChunkedMore !total !n !chunks0 = case n of
  -- If there are no more bytes left in the chunk, we start
  -- on the next decimal-encoded chunk length.
  0 -> parserChunkedMorePost total chunks0
  _ -> do
    b <- Parser.takeUpTo n
    case Bytes.length b of
      -- If there was no input left, we return to request more input.
      -- If we didn't check for this, we would go into a loop.
      0 -> pure (Continuation (More total n) chunks0)
      m -> do
        let chunks1 = ChunksCons b chunks0
        parserChunkedMore total (n - m) chunks1

parserChunkedMorePost :: Int -> Chunks -> Parser HttpException s Continuation
parserChunkedMorePost !total !chunks0 =
  Latin.opt >>= \case
    Just '\r' -> parserChunkedMorePostCr total chunks0
    Just _ -> Parser.fail E.ExpectedCrlfAfterChunk
    Nothing -> pure (Continuation (More total 0) chunks0)

parserChunkedMorePostCr :: Int -> Chunks -> Parser HttpException s Continuation
parserChunkedMorePostCr !total !chunks0 =
  Latin.opt >>= \case
    Just '\n' -> case total of
      0 -> pure (Continuation Done chunks0)
      _ -> parserChunkedChunkLength 0 chunks0
    Just _ -> Parser.fail E.ExpectedCrlfAfterChunk
    Nothing -> pure (Continuation (MorePostCr total) chunks0)

parserChunkedChunkLength :: Word64 -> Chunks -> Parser HttpException s Continuation
parserChunkedChunkLength !acc !chunks0 =
  if acc > 100_000_000
    then Parser.fail E.ChunkTooLarge
    else
      Latin.opt >>= \case
        Nothing -> pure (Continuation (ChunkLength acc) chunks0)
        Just c -> case c of
          '\r' ->
            Latin.opt >>= \case
              Just d -> case d of
                '\n' -> do
                  let !acc' = fromIntegral acc :: Int
                  parserChunkedMore acc' acc' chunks0
                _ -> Parser.fail E.ExpectedCrlfAfterChunkLength
              Nothing -> pure (Continuation (PostCr (fromIntegral acc)) chunks0)
          _ | c >= '0', c <= '9' -> parserChunkedChunkLength (acc * 16 + fromIntegral (ord c - 0x30)) chunks0
          _ | c >= 'a', c <= 'f' -> parserChunkedChunkLength (acc * 16 + fromIntegral (ord c - (0x61 - 10))) chunks0
          _ | c >= 'A', c <= 'F' -> parserChunkedChunkLength (acc * 16 + fromIntegral (ord c - (0x41 - 10))) chunks0
          _ -> Parser.fail E.NonNumericChunkLength

parserChunkedChunkLengthPostCr :: Int -> Chunks -> Parser HttpException s Continuation
parserChunkedChunkLengthPostCr !n !chunks0 =
  Latin.opt >>= \case
    Just d -> case d of
      '\n' -> parserChunkedMore n n chunks0
      _ -> Parser.fail E.ExpectedCrlfAfterChunkLength
    Nothing -> pure (Continuation (PostCr n) chunks0)

-- Note: We could do much better. Upfront, we could allocate a
-- mutable byte array that is big enough to hold the entire body.
-- This would require changing the signature to make a primitive
-- offering reception into mutable byte arrays available.
handleNonchunkedBody :: Resource -> Response -> Bytes -> Headers -> M (Either Exception (Bodied Response))
handleNonchunkedBody ctx resp !post !headers = case lookupContentLength headers of
  Left err -> pure (Left (Http err))
  Right len -> do
    let finish reversedChunks n = case compare n 0 of
          LT -> pure (Left (Http E.PipelinedResponses))
          EQ ->
            pure $
              Right $
                Bodied
                  { metadata = resp
                  , body = Chunks.reverse reversedChunks
                  }
          GT ->
            receive ctx >>= \case
              Right chunk -> case Bytes.length chunk of
                0 -> pure (Left (Http E.NonchunkedBodyEndOfInput))
                _ -> finish (ChunksCons chunk reversedChunks) (n - Bytes.length chunk)
              Left err -> pure (Left (Receive err))
    finish (ChunksCons post ChunksNil) (len - Bytes.length post)

-- This is not great. It relies on the GC to clean up the received
-- bytes for us. It would be better to reuse a mutable byte array
-- and receive into it repeatedly.
discardNonchunkedBody :: Resource -> Response -> Bytes -> Headers -> M (Either Exception Response)
discardNonchunkedBody ctx resp !post !headers = case lookupContentLength headers of
  Left err -> pure (Left (Http err))
  Right len -> do
    let finish n = case compare n 0 of
          LT -> pure (Left (Http E.PipelinedResponses))
          EQ -> pure $ Right $ resp
          GT ->
            receive ctx >>= \case
              Right chunk -> case Bytes.length chunk of
                0 -> pure (Left (Http E.NonchunkedBodyEndOfInput))
                _ -> finish (n - Bytes.length chunk)
              Left err -> pure (Left (Receive err))
    finish (len - Bytes.length post)

splitEndOfHeaders :: Bytes -> Maybe (Bytes, Bytes)
splitEndOfHeaders !b = case Bytes.findTetragramIndex 0x0D 0x0A 0x0D 0x0A b of
  Nothing -> Nothing
  Just n -> Just (Bytes.unsafeTake (n + 4) b, Bytes.unsafeDrop (n + 4) b)

lookupTransferEncoding :: Headers -> Either HttpException TransferEncoding
lookupTransferEncoding !hdrs =
  case Headers.lookupTransferEncoding hdrs of
    Right Header {value} -> case value of
      "chunked" -> Right Chunked
      _ -> Left E.TransferEncodingUnrecognized
    Left Missing -> Right Nonchunked
    Left Duplicate -> Left E.TransferEncodingDuplicated

lookupContentLength :: Headers -> Either HttpException Int
lookupContentLength !hdrs =
  case Headers.lookupContentLength hdrs of
    Left Missing -> Right 0
    Left Duplicate -> Left E.ContentLengthDuplicated
    Right Header {value} -> case readMaybe (T.unpack value) of
      Nothing -> Left E.ContentLengthMalformed
      Just i -> do
        when (i > 8_000_000_000) (Left E.ContentLengthTooLarge)
        Right i

discardChunkedBody ::
  Resource ->
  Response ->
  Bytes ->
  M (Either Exception Response)
discardChunkedBody !ctx resp !input0 = do
  let go :: Instruction -> Bytes -> M (Either Exception Response)
      go instrA !inputA = case Parser.parseBytes (parserChunked (upgradeInstruction instrA)) inputA of
        Parser.Failure e -> pure (Left (Http e))
        Parser.Success (Parser.Slice _ leftoverLen contB) ->
          let instrB = downgradeContinuation contB
           in case leftoverLen of
                -- We expect that parserChunked consumes all input, so we check
                -- here to be certain that it actually does.
                0 -> case instrB of
                  Done -> pure $ Right $ resp
                  _ ->
                    receive ctx >>= \case
                      Right inputB -> case Bytes.length inputB of
                        0 -> pure (Left (Http E.ChunkedBodyEndOfInput))
                        _ -> go instrB inputB
                      Left err -> pure (Left (Receive err))
                _ -> pure (Left (Http E.ImplementationMistake))
  let instr0 = ChunkLength 0
  go instr0 input0

upgradeInstruction :: Instruction -> Continuation
upgradeInstruction i = Continuation i ChunksNil

downgradeContinuation :: Continuation -> Instruction
downgradeContinuation (Continuation i _) = i
