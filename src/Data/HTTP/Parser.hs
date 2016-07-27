module Data.HTTP.Parser (
   H.parseRequestHeaders,
   H.parseResponseHeaders,
   T.RequestHeaders(..),
   T.RequestTarget(..),
   T.ResponseHeaders(..),
) where

import qualified Data.HTTP.Parser.Header as H
import qualified Data.HTTP.Parser.Types as T
