{-# LANGUAGE OverloadedStrings #-}
module Test.Data.HTTP.Parser.ArbitraryInstances where

import qualified Test.QuickCheck as QC

import Data.List(intercalate, subsequences)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BSC
import qualified Network.HTTP.Types as HTTP

import qualified Data.HTTP.Parser.Types as T

instance QC.Arbitrary T.RequestHeaders where
   arbitrary = do
      method <- fmap BSC.pack $ QC.listOf1 . QC.elements $ tchar
      target <- originForm
      version <- QC.elements [HTTP.http11, HTTP.http10]
      headers <- QC.arbitrary
      return $ T.RequestHeaders {
         T.requestMethod = method,
         T.requestTarget = target,
         T.requestVersion = version,
         T.requestHeaders = map getHeader headers
      }
      where
         originForm = do
            let segment = fmap concat $ QC.listOf $ QC.elements pcharswithpercent
            segments <- QC.listOf segment
            query <- fmap getQuery QC.arbitrary
            let path = BSC.pack $ intercalate "/" segments
            return $ T.OriginForm path query

   shrink headers = methodShrunk ++ targetShrunk ++ headersShrunk
      where
         methodShrunk = map (\m -> headers { T.requestMethod = BSC.pack m }) $ methodOptions
         methodOptions = filter (/="") $ QC.shrink $ BSC.unpack $ T.requestMethod headers

         targetShrunk = map (\t -> headers { T.requestTarget = t }) targetOptions
         targetOptions = case T.requestTarget headers of
                           T.OriginForm path query -> shrunkPath path query ++
                                                      shrunkQuery path query
         shrunkPath path query = map (\p -> T.OriginForm (BSC.pack p) query) $ shrinkList $ BSC.unpack path
         shrunkQuery path query = map (T.OriginForm path) $ shrinkList query

         headersShrunk = map (\h -> headers { T.requestHeaders = h }) headersOptions
         headersOptions = shrinkList $ T.requestHeaders headers

         shrinkList xs = map removeNth [0..len]
            where
               len = (length xs) - 1
               removeNth n = let (pre, post) = splitAt n xs
                             in pre ++ (drop 1 post)

instance QC.Arbitrary T.ResponseHeaders where
   arbitrary = do
      version <- QC.elements [HTTP.http11, HTTP.http10]
      statusCode <- QC.resize 899 QC.arbitrarySizedNatural
      statusMessage <- fmap BSC.pack $ QC.listOf . QC.elements $ headerValChar
      let status = HTTP.mkStatus (100 + statusCode) statusMessage
      headers <- QC.arbitrary
      return $ T.ResponseHeaders {
         T.responseVersion = version,
         T.responseStatus = status,
         T.responseHeaders = map getHeader headers
      }

instance QC.Arbitrary T.MultipartForm where
   arbitrary = do
      sep <- (fmap BSC.pack) . QC.listOf1 $ QC.elements ['!'..'~']
      chunks <- fmap ((map getChunk) . filterContains sep) QC.arbitrary
      return $ T.MultipartForm {
         T.multipartFormSeparator = sep,
         T.multipartFormChunks = chunks
      }
      where
         filterContains sep = filter (contains)
            where
               contains chunk = let (_, body) = getChunk chunk
                                in hasSep body
               hasSep = (== BSC.pack "") . snd . BSC.breakSubstring fullSep
               fullSep = BSC.append (BSC.pack "--") sep

   shrink form = map (T.MultipartForm sep) shrunkChunks
      where
         shrunkChunks = QC.shrinkList (map getChunk . QC.shrink . ArbMultipartFormChunk) $ chunks

         sep = T.multipartFormSeparator form
         chunks = T.multipartFormChunks form

-- Wrap it so we don't take over the third party type
newtype HTTPHeader = HTTPHeader { getHeader :: HTTP.Header }
instance QC.Arbitrary HTTPHeader where
   arbitrary = do
      name <- fmap (CI.mk . BSC.pack) $ QC.listOf1 . QC.elements $ tchar
      valueBody <- QC.listOf1 . QC.elements $ headerValChar
      value <- case valueBody of
                    val@(' ':rest) -> do
                       leadingChar <- QC.elements headerValChar
                       return $ leadingChar:val
                    anything -> return anything
      return $ HTTPHeader (name, BSC.pack value)

-- Wrap it so we don't take over the third party type
newtype HTTPQuery  = HTTPQuery { getQuery :: HTTP.Query }
instance QC.Arbitrary HTTPQuery where
   arbitrary = fmap HTTPQuery $ QC.listOf queryElement
      where
         queryElement = do
            firstKeyChar <- QC.arbitrary
            key <- fmap (BSC.pack . (firstKeyChar:)) $ QC.arbitrary
            value <- QC.oneof [fmap (Just . BSC.pack) $ QC.arbitrary,
                               return Nothing] 
            return (key, value)

newtype ArbMultipartFormChunk = ArbMultipartFormChunk { getChunk :: T.MultipartFormChunk }
instance QC.Arbitrary ArbMultipartFormChunk where
   arbitrary = do
      -- TODO: This name field is too broad, some characters (e.g. ") are percent encoded
      headers <- fmap (map getHeader) $ QC.arbitrary
      body <- fmap BSC.pack QC.arbitrary
      name <- QC.listOf . QC.elements $ ['\0'..'\9'] ++ "\11\12" ++ ['\14'..'\255']
      -- TODO: This isn't random enough; need spaces and case insensitivity
      let cdBody = BSC.pack $ concat ["form-data; name=\"", name, "\""]
          contentDisposition = (CI.mk . BSC.pack $ "Content-Disposition", cdBody)
          fullHeaders = contentDisposition:headers
      return $ ArbMultipartFormChunk (fullHeaders, body)

   shrink wrapper = map (\a -> ArbMultipartFormChunk (a, content)) $ headerSubsets
      where
         headerSubsets = filter hasOneContentDisposition $ filterEqual $ subsequences headers
         filterEqual = filter (/= headers)
         hasOneContentDisposition headerSet = cdCount == 1
            where
               cdCount = length $ filter isContentDisposition headerSet
         isContentDisposition (name, _) = CI.mk "Content-Disposition" == name
         (headers, content) = getChunk wrapper

-- String sets from the spec: https://tools.ietf.org/html/rfc7230
tchar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!#$%&'*+-.^_`|~"
headerValChar = ['\x80'..'\xff'] ++ ['\x21'..'\x7e']
headerValCharWithWhitespace = " \t" ++ headerValChar
pcharswithpercent = simple ++ percents
   where
      simple = map (:[]) pchars 
      percents = [['%', hex!!d1, hex!!d2] | d1 <- [0..15], d2 <- [0..15]]
      hex = "0123456789abcdef"
pchars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ":@-._~!$&'()*+,;="
