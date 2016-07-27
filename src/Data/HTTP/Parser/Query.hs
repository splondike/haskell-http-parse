{-# LANGUAGE OverloadedStrings #-}
module Data.HTTP.Parser.Query (
   parseQuery,
   renderQuery
) where

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTTP
import qualified Data.Attoparsec.ByteString.Char8 as AP

parseQuery :: AP.Parser HTTP.Query
parseQuery = HTTP.parseQuery <$> AP.takeWhile allowedChars

renderQuery :: HTTP.Query -> BS.ByteString
renderQuery = HTTP.renderQuery False

allowedChars :: Char -> Bool
allowedChars = AP.inClass $ "-?/:@._~!$&'()*+,;=%a-zA-Z0-9"
