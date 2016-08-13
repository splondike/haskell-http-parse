{-# LANGUAGE OverloadedStrings #-}
module Data.HTTP.Parser.Body (
   -- Parsing
   parseUrlEncoded,
   urlEncodedFormParser,
   parseMultipart,
   multipartFormParser,

   -- Rendering
   renderUrlEncoded,
   renderMultipart
) where

import Control.Applicative((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Network.HTTP.Types as HTTP
import qualified Data.CaseInsensitive as CI

import qualified Data.HTTP.Parser.Header as H
import qualified Data.HTTP.Parser.Query as Q
import qualified Data.HTTP.Parser.Types as T

parseUrlEncoded :: BS.ByteString -> Either String HTTP.Query
parseUrlEncoded = AP.parseOnly urlEncodedFormParser

urlEncodedFormParser :: AP.Parser HTTP.Query
urlEncodedFormParser = Q.parseQuery

-- Parser for https://tools.ietf.org/html/rfc2388
parseMultipart :: BS.ByteString -> BS.ByteString -> Either String T.MultipartForm
parseMultipart separator = AP.parseOnly (multipartFormParser separator)

-- TODO: Don't require this parser to consume all the input (endOfInput), it should
-- just finish when it sees the finalLine
multipartFormParser :: BS.ByteString -> AP.Parser T.MultipartForm
multipartFormParser separator = T.MultipartForm separator <$> (withContent <|> noContent)
                                                          <*  AP.endOfInput
   where
      withContent = (separatorLine *> AP.many' formChunk)
      noContent = finalLine *> pure []
      formChunk = do
         headers <- H.parseHttpHeaders
         _ <- newLine
         body <- bodyParser
         return $ (headers, body)
      -- Any arbitrary characters up until we hit a separator or the final line. Keep
      -- taking characters until we hit the start of a CRLF. Then check if we've hit
      -- a separator, and if so, consume that terminator and finish. If not, keep
      -- consuming input
      -- bodyParser = BS.append <$> AP.takeWhile (/= '\r') <*> (endOfChunk <|> bodyParser)
      bodyParser = BSC.pack <$> AP.manyTill AP.anyChar endOfChunk

      endOfChunk = newLine *> (separatorLine <|> finalLine) *> pure ""

      -- Each chunk is separated by one of these
      separatorLine = AP.string fullSep *> newLine
      fullSep = BS.append "--" separator
      -- The end of the document is one of these
      finalLine = AP.string fullSep *> AP.string "--" *> newLine 

newLine :: AP.Parser BS.ByteString
newLine = AP.string "\r\n"

renderUrlEncoded :: HTTP.Query -> BS.ByteString
renderUrlEncoded = Q.renderQuery

renderMultipart :: T.MultipartForm -> BS.ByteString
renderMultipart mpForm = BS.concat $ (map render chunks) ++ endSep
   where
      render (headers, body) = BS.concat $ [sep, nl] ++ (map renderH headers) ++ [nl, body, nl]
      renderH (name, value) = BS.concat [CI.original name, ":", value, nl]

      sep = BS.append "--" $ T.multipartFormSeparator mpForm
      endSep = [sep, "--", nl]
      chunks = T.multipartFormChunks mpForm
      nl = "\r\n"
