{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An API for the Online Etymology Dictionary at http://etymonline.com/.
--   The encoding is ISO-8859-1/latin1 so needs to use uconv.

module Language.English.EtymologyOnline where

import           Control.Exception
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as Bytes
import           Data.ByteString.UTF8  (toString)
import           Data.Maybe
import           Data.Text.ICU.Convert
import           Data.Typeable
import           Network.Curl
import           System.Random
import           Text.XML.Light

-- | An EO exception.
data EOException
  = CannotParsePageCountForLetter Char
  | CannotDownloadPageCountForLetter Char
  | CannotParsePageEntries Char Int (Maybe Element)
  | CannotDownloadPageEntries Char Int
  | NoEntryInPage Char Int
  deriving (Typeable,Show)
instance Exception EOException

-- | Get a random Etymology Online entry.
getRandomEntry :: IO (String,String)
getRandomEntry = do
  converter <- open "utf-8" Nothing
  letter <- randomRIO ('a','z')
  pages <- getletterpages converter letter
  pagenumber <- randomRIO (1,pages)
  entries <- getletterentries converter letter pagenumber
  index <- randomRIO (1,length entries)
  maybe (throw (NoEntryInPage letter pagenumber))
        return
        (lookup index (zip [1..] entries))

-- | Get the number of pages for a letter.
getletterpages :: Converter -> Char -> IO Int
getletterpages converter l = withCurlDo $ do
  (code,bytes) <- curlGetString_ url []
  case code of
    CurlOK -> do
      let doc = parseEOXML converter (bytes :: ByteString)
      maybe (return 1) -- Some pages don't have pagination at all.
            (return . length)
            (getlinks doc)
    _ -> throw (CannotDownloadPageCountForLetter l)
  where url = "http://www.etymonline.com/index.php?l=" ++ [l]

-- | Get the entries for a letter and page number.
getletterentries :: Converter -> Char -> Int -> IO [(String,String)]
getletterentries converter l pn = withCurlDo $ do
  (code,bytes) <- curlGetString_ url []
  case code of
    CurlOK -> do
      let doc = parseEOXML converter (bytes :: ByteString)
      maybe (throw (CannotParsePageEntries l pn doc)) return (getentries doc)
    _ -> throw (CannotDownloadPageEntries l pn)
  where url = "http://www.etymonline.com/index.php?l=" ++ [l] ++ "&p=" ++ show (pn - 1)

-- Get the entries given the page.
getentries :: Maybe Element -> Maybe [(String, String)]
getentries doc = do
  el <- doc
  let dts = grab "dt" el
      dds = grab "dd" el
  return (zip dts dds)

  where grab key = map (trim . allContent)
                 . findElements (qname key xhtml)

-- Get the pagination links from the page.
getlinks :: Maybe Element -> Maybe [String]
getlinks doc = do
  el <- doc
  paging <- filterElement paginglist el
  ul <- findChild (qname "ul" xhtml) paging
  return (paginglinks ul)

-- Find the element with the pagination.
paginglist :: Element -> Bool
paginglist el = findAttr (qname "class" Nothing) el == Just "paging"

-- Get the pagination links.
paginglinks :: Element -> [String]
paginglinks = mapMaybe (findAttr (qname "href" Nothing))
            . getels "a"

  where getels key = findElements (qname key xhtml)

-- Get all text content of an element.
allContent :: Element -> String
allContent (Element _ _ contents _) = concatMap switch contents where
  switch el =
    case el of
      Elem e -> allContent e
      CRef _  -> ""
      Text (CData _ d _) -> d

-- Parse an ISO-8859-1 document to an XML tree.
parseEOXML :: Converter -> ByteString -> Maybe Element
parseEOXML converter =
  parseXMLDoc . toUnicode converter

-- Some utils.

xhtml :: Maybe String
xhtml = Just "http://www.w3.org/1999/xhtml"

qname :: String -> Maybe String -> QName
qname q uri = blank_name { qName = q, qURI = uri }

trim :: String -> String
trim = unwords . words
