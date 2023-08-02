{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Exchange
  ( Exception(..)
  , exchange
  ) where

import Channel (M,Resource,SendException,ReceiveException,send,receive)
import Control.Monad (when)
import Data.Char (ord)
import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks(ChunksCons,ChunksNil))
import Data.Primitive (SmallArray)
import Data.Word (Word64)
import Http.Bodied (Bodied(Bodied))
import Http.Exchange.Types (HttpException)
import Http.Header (Header(Header))
import Http.Message.Request (Request)
import Http.Message.Response (Response)
import Text.Read (readMaybe)
import Data.Bytes.Parser (Parser)

import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Http.Exchange.Types as E
import qualified Data.List as List
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Text as T
import qualified Http.Header
import qualified Http.Message.Request as Request
import qualified Http.Message.Response as Response
import qualified Http.Bodied

data Continuation = Continuation
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
  | Done
    -- ^ We got all the chunks, and we got the zero-length chunk
    -- at the end, and we got the trailing CRLF. We are done.

data TransferEncoding
  = Nonchunked
  | Chunked

-- | An exception that occurs during an HTTP exchange.
data Exception
  = Http -- ^ The response was not a valid HTTP response
      !HttpException
  | Send
      -- ^ Transport exception while sending. When backed by stream sockets,
      -- exceptions like @ECONNRESET@ show up here.
      !SendException
  | Receive
      -- ^ Transport exception while receiving. Depending on the backend,
      -- this may or may not include an end-of-input exception. For stream
      -- sockets, end-of-input is not presented as an exception. It is
      -- presented as a zero-length result.
      !ReceiveException

exchange ::
     Resource
  -> Bodied Request -- http request line and headers
  -> M (Either Exception (Bodied Response))
exchange ctx req = do
  let enc = Request.bodiedToChunks req
  send ctx enc >>= \case
    Left err -> pure (Left (Send err))
    Right () -> receiveResponse ctx

receiveResponse ::
     Resource
  -> M (Either Exception (Bodied Response))
receiveResponse !ctx = do
  let go !oldOutput = receive ctx >>= \case
        Left err -> pure (Left (Receive err))
        Right newOutput -> case Bytes.length newOutput of
          0 -> pure (Left (Http E.HeadersEndOfInput))
          _ -> do
            let output = oldOutput <> newOutput
            case splitEndOfHeaders output of
              Nothing -> if Bytes.length output > 16000
                then pure (Left (Http E.HeadersTooLarge))
                else go output
              Just (pre,post) -> case Response.decode 128 pre of
                Nothing -> pure (Left (Http E.HeadersMalformed))
                Just resp@Response.Response{headers} -> case lookupTransferEncoding headers of
                  Left err -> pure (Left (Http err))
                  Right enc -> case enc of
                    Nonchunked -> handleNonchunkedBody ctx resp post headers
                    Chunked -> handleChunkedBody ctx resp post
  go mempty

handleChunkedBody ::
     Resource
  -> Response
  -> Bytes
  -> M (Either Exception (Bodied Response))
handleChunkedBody !ctx resp !input0 = do
  let go contA !inputA = case Parser.parseBytes (parserChunked contA) inputA of
        Parser.Failure e -> pure (Left (Http e))
        Parser.Success (Parser.Slice _ leftoverLen contB) -> case leftoverLen of
          -- We expect that parserChunked consumes all input, so we check
          -- here to be certain that it actually does.
          0 -> case contB of
            Continuation Done revChunks -> pure $ Right $ Bodied
              { metadata = resp
              , body = Chunks.reverse revChunks
              }
            _ -> receive ctx >>= \case
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
parserChunkedMorePost !total !chunks0 = Latin.opt >>= \case
  Just '\r' -> parserChunkedMorePostCr total chunks0
  Just _ -> Parser.fail E.ExpectedCrlfAfterChunk
  Nothing -> pure (Continuation (More total 0) chunks0)

parserChunkedMorePostCr :: Int -> Chunks -> Parser HttpException s Continuation
parserChunkedMorePostCr !total !chunks0 = Latin.opt >>= \case
  Just '\n' -> case total of
    0 -> pure (Continuation Done chunks0)
    _ -> parserChunkedChunkLength 0 chunks0
  Just _ -> Parser.fail E.ExpectedCrlfAfterChunk
  Nothing -> pure (Continuation (MorePostCr total) chunks0)

parserChunkedChunkLength :: Word64 -> Chunks -> Parser HttpException s Continuation
parserChunkedChunkLength !acc !chunks0 = if acc > 100_000_000
  then Parser.fail E.ChunkTooLarge
  else Latin.opt >>= \case
    Nothing -> pure (Continuation (ChunkLength acc) chunks0)
    Just c -> case c of
      '\r' -> Latin.opt >>= \case
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
parserChunkedChunkLengthPostCr !n !chunks0 = Latin.opt >>= \case
  Just d -> case d of
    '\n' -> parserChunkedMore n n chunks0
    _ -> Parser.fail E.ExpectedCrlfAfterChunkLength
  Nothing -> pure (Continuation (PostCr n) chunks0)

handleNonchunkedBody :: Resource -> Response -> Bytes -> SmallArray Header -> M (Either Exception (Bodied Response))
handleNonchunkedBody ctx resp !post !headers = case lookupContentLength headers of
  Left err -> pure (Left (Http err))
  Right len -> do
    let finish reversedChunks n = case compare n 0 of
          LT -> pure (Left (Http E.PipelinedResponses))
          EQ -> pure $ Right $ Bodied
            { metadata = resp
            , body = Chunks.reverse reversedChunks
            }
          GT -> receive ctx >>= \case
            Right chunk -> case Bytes.length chunk of
              0 -> pure (Left (Http E.NonchunkedBodyEndOfInput))
              _ -> finish (ChunksCons chunk reversedChunks) (n - Bytes.length chunk)
            Left err -> pure (Left (Receive err))
    finish (ChunksCons post ChunksNil) (len - Bytes.length post)

splitEndOfHeaders :: Bytes -> Maybe (Bytes, Bytes)
splitEndOfHeaders !b = case Bytes.findTetragramIndex 0x0D 0x0A 0x0D 0x0A b of
  Nothing -> Nothing
  Just n -> Just (Bytes.unsafeTake (n + 4) b, Bytes.unsafeDrop (n + 4) b)

lookupTransferEncoding :: SmallArray Header -> Either HttpException TransferEncoding
lookupTransferEncoding !hdrs =
  case List.find (\Header{name} -> T.toLower name == "transfer-encoding") hdrs of
    Just Header{value} -> case value of
      "chunked" -> Right Chunked
      _ -> Left E.UnrecognizedTransferEncoding
    Nothing -> Right Nonchunked

lookupContentLength :: SmallArray Header -> Either HttpException Int
lookupContentLength !hdrs =
  case List.find (\Header{name} -> T.toLower name == "content-length") hdrs of
    Nothing -> Right 0
    Just Header{value} -> case readMaybe (T.unpack value) of
      Nothing -> Left E.ContentLengthMalformed
      Just i -> do
        when (i > 8_000_000_000) (Left E.ContentLengthTooLarge)
        Right i
