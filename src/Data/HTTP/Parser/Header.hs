{-
 - Implements a parser for the HTTP request/response line and headers part
 - of a HTTP request. See https://tools.ietf.org/html/rfc7230
 -}
{-# LANGUAGE OverloadedStrings #-}
module Data.HTTP.Parser.Header (
   -- Parsing
   parseRequestHeaders,
   requestHeaderParser,
   parseResponseHeaders,
   responseHeaderParser,
   parseHttpHeaders,
   parseHttpHeader,

   -- Rendering
   renderRequestHeaders,
   renderResponseHeaders,
   renderHttpHeaders,
   renderHttpHeader
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Types as HTTP
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.CaseInsensitive as CI

import qualified Data.HTTP.Parser.Types as T
import qualified Data.HTTP.Parser.Query as Q

parseRequestHeaders :: BS.ByteString -> Either String T.RequestHeaders
parseRequestHeaders = AP.parseOnly requestHeaderParser

requestHeaderParser :: AP.Parser T.RequestHeaders
requestHeaderParser = parsedRequestHeader
   where
      parsedRequestHeader = T.RequestHeaders <$> (method <?> "couldn't parse method")
                                             <*  (AP.char ' ' <?> "no space after method")
                                             <*> (requestTarget <?> "couldn't parse request target")
                                             <*  (AP.char ' ' <?> "no space after request target")
                                             <*> (parseVersion <?> "couldn't parse version string")
                                             <*  (newLine <?> "no newline after version")
                                             <*> (parseHttpHeaders <?> "couldn't parse headers")
                                             <*  (newLine <?> "no CRLF after headers")
      method = AP.takeWhile1 ((flip elem) methodChar)
      -- TODO: Try parsing other RequestTargets here

      requestTarget = T.OriginForm <$> path <*> (AP.option [] query)
      path = AP.char '/' *> AP.takeWhile ((flip elem) pathChar)
      query = AP.char '?' *> Q.parseQuery

parseResponseHeaders :: BS.ByteString -> Either String T.ResponseHeaders
parseResponseHeaders = AP.parseOnly responseHeaderParser

responseHeaderParser :: AP.Parser T.ResponseHeaders
responseHeaderParser = parsedResponseHeaders
   where
      parsedResponseHeaders = T.ResponseHeaders <$> (parseVersion <?> "couldn't parse version string")
                                                <*  (AP.char ' ' <?> "no space after version")
                                                <*> (status <?> "couldn't parse status string")
                                                <*> (parseHttpHeaders <?> "couldn't parse headers")
                                                <*  (newLine <?> "no CRLF after headers")
      status = do
         num <- fmap read $ AP.many1 digit
         desc <- fmap BSC.pack $ AP.manyTill AP.anyChar newLine
         return $ HTTP.mkStatus num desc
      digit = AP.satisfy (\c -> c >= '0' && c <= '9')

parseVersion :: AP.Parser HTTP.HttpVersion
parseVersion = AP.string "HTTP/" *> AP.choice [AP.string "1.0" *> pure HTTP.http10,
                                          AP.string "1.1" *> pure HTTP.http11]

parseHttpHeaders :: AP.Parser [HTTP.Header]
parseHttpHeaders = AP.many' parseHttpHeader
parseHttpHeader :: AP.Parser HTTP.Header
parseHttpHeader = do
   key <- AP.takeWhile1 headerKeyChar
   _ <- AP.string ":"
   _ <- AP.many' (AP.char ' ')
   let leadingSpaceLine = AP.many1 (AP.char ' ') *> 
                          (fmap BSC.pack $ AP.manyTill AP.anyChar newLine)
   -- TODO: ltrim the concatenated value
   -- HTTP/1.1 actually doesn't allow multi-line headers, but we're going to be
   -- lenient
   val <- (\x y -> BS.concat ((BSC.pack x):y)) <$> AP.manyTill AP.anyChar newLine
                                              <*> AP.many' leadingSpaceLine
   return (CI.mk key, val)

renderRequestHeaders :: T.RequestHeaders -> BS.ByteString
renderRequestHeaders req = BS.concat [method, " ", target, " ", version, "\r\n",
                                      renderHttpHeaders $ T.requestHeaders req, "\r\n"]
   where
      method = T.requestMethod req
      target = case T.requestTarget req of
                    T.OriginForm path [] -> BS.concat ["/", path]
                    T.OriginForm path query -> BS.concat ["/", path, "?", Q.renderQuery query]
      version = BSC.pack $ show $ T.requestVersion req

renderResponseHeaders :: T.ResponseHeaders -> BS.ByteString
renderResponseHeaders resp = BS.concat [version, " ", status, "\r\n",
                                        renderHttpHeaders $ T.responseHeaders resp, "\r\n"]
   where
      version = BSC.pack $ show $ T.responseVersion resp
      status = BS.concat [BSC.pack . show $ HTTP.statusCode st, " ", HTTP.statusMessage st]
      st = T.responseStatus resp

renderHttpHeaders :: [HTTP.Header] -> BS.ByteString
renderHttpHeaders headers = BS.concat $ map renderHttpHeader headers

renderHttpHeader :: HTTP.Header -> BS.ByteString
renderHttpHeader (name, value) = BS.concat [CI.original name, ":", value, "\r\n"]

headerKeyChar :: Char -> Bool
headerKeyChar = AP.inClass $ "!#$%&'*+-.^_`|~" ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
newLine :: AP.Parser BS.ByteString
newLine = AP.string "\r\n"

methodChar :: String
methodChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-!#$%&'*+.^_`|~"
pathChar :: String
pathChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-:@._~!$&'()*+,;=%/"
