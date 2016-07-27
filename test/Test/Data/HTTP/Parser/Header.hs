{-# LANGUAGE OverloadedStrings #-}
module Test.Data.HTTP.Parser.Header (tests) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import qualified Test.QuickCheck as QC

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Types as HTTP
import qualified Data.CaseInsensitive as CI

import Test.Data.HTTP.Parser.ArbitraryInstances ()
import qualified Data.HTTP.Parser.Header as P
import qualified Data.HTTP.Parser.Types as T

tests = testGroup "Header parsing tests" [parseGETRequestHeaders,
                                          parseGETResponseHeaders,
                                          propRenderResponseMatchesParse,
                                          propRenderRequestMatchesParse]

parseGETRequestHeaders = testCase "Parse GET request" $ parseResult @?= expectedResult
   where
      parseResult = P.parseRequestHeaders validGetRequest
      expectedResult = Right $ T.RequestHeaders {
         T.requestMethod = "GET",
         T.requestVersion = HTTP.http11,
         T.requestTarget = T.OriginForm "cool/request/" [("hello", Nothing), ("bye", Just "he llo")],
         T.requestHeaders = toCI [
            ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"),
            ("Accept-Encoding", "gzip, deflate, sdch"),
            ("Accept-Language", "en-GB,en-US;q=0.8,en;q=0.6"),
            ("Connection", "keep-alive"),
            ("Host", "www.google.com.au"),
            ("Upgrade-Insecure-Requests", "1"),
            ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.63 Safari/537.36")
         ]
      }
      toCI = map (\(h, v) -> (CI.mk h, v))

parseGETResponseHeaders = testCase "Parse GET response" $ parseResult @?= expectedResult
   where
      parseResult = P.parseResponseHeaders validGetResponse
      expectedResult = Right $ T.ResponseHeaders {
         T.responseVersion = HTTP.http11,
         T.responseStatus = HTTP.mkStatus 302 "Found",
         T.responseHeaders = toCI [
            ("Cache-Control", "private"),
            ("Content-Type", "text/html; charset=UTF-8"),
            ("Location", "http://www.google.com.au/?gfe_rd=cr&ei=LTFMV-HSL9Hu8wffioGgDg"),
            ("Content-Length", "262"),
            ("Date", "Mon, 30 May 2016 12:25:17 GMT")
         ]
      }
      toCI = map (\(h, v) -> (CI.mk h, v))

propRenderRequestMatchesParse = 
   testProperty "Rendered request header matches parse result" $ test
   where
      test request = renderParse request QC.=== (Right request)
      renderParse = P.parseRequestHeaders . P.renderRequestHeaders

propRenderResponseMatchesParse = 
   testProperty "Rendered response header matches parse result" $ test
   where
      test response = renderParse response QC.=== (Right response)
      renderParse = P.parseResponseHeaders . P.renderResponseHeaders

validGetRequest = mkCase [
   "GET /cool/request/?hello&bye=he%20llo HTTP/1.1",
   "Accept:text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
   "Accept-Encoding:gzip, deflate, sdch",
   "Accept-Language:en-GB,en-US;q=0.8,en;q=0.6",
   "Connection:keep-alive",
   "Host:www.google.com.au",
   "Upgrade-Insecure-Requests:1",
   "User-Agent:Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.63 Safari/537.36",
   ""]

validGetResponse = mkCase [
   "HTTP/1.1 302 Found",
   "Cache-Control: private",
   "Content-Type: text/html; charset=UTF-8",
   "Location: http://www.google.com.au/?gfe_rd=cr&ei=LTFMV-HSL9Hu8wffioGgDg",
   "Content-Length: 262",
   "Date: Mon, 30 May 2016 12:25:17 GMT",
   ""]

mkCase = BS.concat . (map (\x -> BS.concat [x, "\r\n"]))
