module Data.HTTP.Parser.Types where 

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTTP

data RequestHeaders = RequestHeaders {
   requestMethod :: HTTP.Method,
   requestTarget :: RequestTarget,
   requestVersion :: HTTP.HttpVersion,
   requestHeaders :: HTTP.RequestHeaders
} deriving (Show, Eq, Ord)

data ResponseHeaders = ResponseHeaders {
   responseVersion :: HTTP.HttpVersion,
   responseStatus :: HTTP.Status,
   responseHeaders :: HTTP.ResponseHeaders
} deriving (Show, Eq, Ord)

data MultipartForm = MultipartForm {
   multipartFormSeparator :: BS.ByteString,
   multipartFormChunks :: [MultipartFormChunk]
} deriving (Show, Eq, Ord)

type MultipartFormChunk = ([HTTP.Header], BS.ByteString)

-- | The various request formats that are permitted after
-- the method in a HTTP 1.1 request.
data RequestTarget = OriginForm BS.ByteString HTTP.Query
                     -- The following three forms are not currently supported
                     -- AbsoluteForm |
                     -- AuthorityForm |
                     -- AsteriskForm
                     deriving (Show, Eq, Ord)
