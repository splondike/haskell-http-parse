{-# LANGUAGE OverloadedStrings #-}
module Test.Data.HTTP.Parser.Body (tests) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import qualified Test.QuickCheck as QC

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.CaseInsensitive as CI

import Test.Data.HTTP.Parser.ArbitraryInstances ()
import qualified Data.HTTP.Parser.Body as P
import qualified Data.HTTP.Parser.Types as T

tests = testGroup "Body parsing tests" [parseMultiPartFormData,
                                        propRenderMulipartMatchesParse]

parseMultiPartFormData = testCase "Parse multipart data" $ parseResult  @?= expectedResult
   where
      parseResult = P.parseMultipart separator validMultipart
      separator = "----WebKitFormBoundarysb2LOHnpyhoDV0Wf"
      expectedResult = Right $ T.MultipartForm {
         T.multipartFormSeparator = separator,
         T.multipartFormChunks = [
            (toCI [
               ("Content-Disposition", "form-data; name=\"nombre\"")
             ], "Hilo"),
            (toCI [
               ("Content-Disposition", "form-data; name=\"archivos\"; filename=\"file1.txt\""),
               ("Content-Type", "text/plain")
             ], "file\n"),
            (toCI [
               ("Content-Disposition", "form-data; name=\"archivos\"; filename=\"file2.txt\""),
               ("Content-Type", "text/plain")
             ], "file\n")
         ]
      }
      toCI = map (\(h, v) -> (CI.mk h, v))

propRenderMulipartMatchesParse =
   testProperty "Rendered multipart form request matches parse result" $ test
   where
      test request = renderParse request QC.=== (Right request)
      renderParse req = P.parseMultipart sep $ (P.renderMultipart req)
         where
            sep = T.multipartFormSeparator req

validMultipart = BS.concat [
   "------WebKitFormBoundarysb2LOHnpyhoDV0Wf\r\n",
   "Content-Disposition: form-data; name=\"nombre\"\r\n",
   "\r\n",
   "Hilo\r\n",
   "------WebKitFormBoundarysb2LOHnpyhoDV0Wf\r\n",
   "Content-Disposition: form-data; name=\"archivos\"; filename=\"file1.txt\"\r\n",
   "Content-Type: text/plain\r\n",
   "\r\n",
   "file\n",
   "\r\n",
   "------WebKitFormBoundarysb2LOHnpyhoDV0Wf\r\n",
   "Content-Disposition: form-data; name=\"archivos\"; filename=\"file2.txt\"\r\n",
   "Content-Type: text/plain\r\n",
   "\r\n",
   "file\n",
   "\r\n",
   "------WebKitFormBoundarysb2LOHnpyhoDV0Wf--\r\n"]
