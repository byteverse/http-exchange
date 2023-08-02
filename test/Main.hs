{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

import Test.Tasty (TestTree,testGroup,defaultMain)
import Test.Tasty.HUnit (testCase,(@=?))
import Data.Bytes (Bytes)
import Http.Message.Request (Request(Request),RequestLine(RequestLine))
import Http.Bodied (Bodied(Bodied))
import Http.Header (Header(Header))
import OkChannel (M(M))
import Data.Bytes.Chunks (Chunks(ChunksCons,ChunksNil))
import Http.Message.Response (Response)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified GHC.Exts as Exts
import qualified OkExchange as E
import qualified Http.Header
import qualified Http.Bodied
import qualified Http.Message.Request as Request
import qualified Data.Bytes.Text.Ascii as Ascii

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testCase "get-a" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (Chunks.fromBytes getRespA)
      body @=? ChunksNil
      input @=? mempty
      output @=? Chunks.concat (Request.bodiedToChunks getReqA)
  , testCase "get-chunked-a" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (Chunks.fromBytes getRespChunkedA)
      body @=? ChunksNil
      input @=? mempty
      output @=? Chunks.concat (Request.bodiedToChunks getReqA)
  , testCase "get-chunked-byte-by-byte-a" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (bytesToSingleByteChunks getRespChunkedA)
      body @=? ChunksNil
      input @=? mempty
      output @=? Chunks.concat (Request.bodiedToChunks getReqA)
  , testCase "get-body-a" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (Chunks.fromBytes getRespBodyA)
      input @=? mempty
      body @=? ChunksCons (Ascii.fromString "helloworld") ChunksNil
      output @=? Chunks.concat (Request.bodiedToChunks getReqA)
  , testCase "get-chunked-b" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (Chunks.fromBytes getRespChunkedB)
      Ascii.fromString "hello to my friends." @=? Chunks.concat body
      mempty @=? input
      Chunks.concat (Request.bodiedToChunks getReqA) @=? output
  , testCase "get-chunked-byte-by-byte-b" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (bytesToSingleByteChunks getRespChunkedB)
      Ascii.fromString "hello to my friends." @=? Chunks.concat body
      mempty @=? input
      Chunks.concat (Request.bodiedToChunks getReqA) @=? output
  , testCase "get-chunked-two-by-two-b" $ do
      (input,output,Bodied{body}) <- evaluateM (E.exchange () getReqA) (bytesToDoubletonByteChunks getRespChunkedB)
      Ascii.fromString "hello to my friends." @=? Chunks.concat body
      mempty @=? input
      Chunks.concat (Request.bodiedToChunks getReqA) @=? output
  ]

bytesToSingleByteChunks :: Bytes -> Chunks
bytesToSingleByteChunks = Bytes.foldr'
  (\w acc -> ChunksCons (Bytes.singleton w) acc
  ) ChunksNil

bytesToDoubletonByteChunks :: Bytes -> Chunks
bytesToDoubletonByteChunks b0 = go (Exts.toList b0)
  where
  go (x : y : zs) = ChunksCons (Exts.fromList [x,y]) (go zs)
  go [x] = ChunksCons (Bytes.singleton x) ChunksNil
  go [] = ChunksNil

evaluateM ::
     M (Either E.Exception (Bodied Response))
  -> Chunks -- prebuilt response
  -> IO (Chunks,Bytes,Bodied Response)
evaluateM (M f) resp = case f resp mempty of
  (input,output,r) -> case r of
    Left e -> case e of
      E.Http err -> fail ("exchange http protocol failure: " ++ show err)
      E.Transport err -> fail ("exchange transport failure: " ++ show err)
    Right b -> pure (input,output,b)

getReqA :: Bodied Request
getReqA = Bodied
  { metadata = Request
    { requestLine=RequestLine
      { method = "GET"
      , path = "/health"
      , versionMajor = 1
      , versionMinor = 1
      }
    , headers = Exts.fromList
      [ Header{name="Host",value="example.com"}
      ]
    }
  , body = mempty
  }

getRespA :: Bytes
getRespA = Ascii.fromString
  "HTTP/1.1 200 OK\r\n\
  \Server: testsuite/1.2.3\r\n\
  \Content-Type: text/plain\r\n\r\n"

getRespBodyA :: Bytes
getRespBodyA = Ascii.fromString
  "HTTP/1.1 200 OK\r\n\
  \Server: testsuite/1.2.3\r\n\
  \Content-Type: text/plain\r\n\
  \Content-Length: 10\r\n\r\n\
  \helloworld"

getRespChunkedA :: Bytes
getRespChunkedA = Ascii.fromString
  "HTTP/1.1 200 OK\r\n\
  \Server: testsuite/1.2.3\r\n\
  \Transfer-Encoding: chunked\r\n\
  \Content-Type: text/plain\r\n\r\n\
  \0\r\n\r\n"

getRespChunkedB :: Bytes
getRespChunkedB = Ascii.fromString
  "HTTP/1.1 200 OK\r\n\
  \Server: testsuite/1.2.3\r\n\
  \Transfer-Encoding: chunked\r\n\
  \Content-Type: text/plain\r\n\r\n\
  \5\r\nhello\r\n\
  \f\r\n to my friends.\r\n\
  \0\r\n\r\n"
