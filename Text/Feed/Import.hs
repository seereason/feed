--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Import
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
--
-- Convert from XML to Feeds.
--
--------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Text.Feed.Import
        ( parseFeedFromFile -- :: FilePath -> IO Feed
        , parseFeedString   -- :: String -> IO Feed

          -- if you know your format, use these directly:
	, readRSS2          -- :: XML.Element -> Maybe Feed
	, readRSS1          -- :: XML.Element -> Maybe Feed
	, readAtom          -- :: XML.Element -> Maybe Feed
        ) where

import Text.Atom.Feed.Import as Atom
import Text.RSS.Import       as RSS
import Text.RSS1.Import      as RSS1

import Text.Feed.Types
import Text.XML.Light as XML
import Text.XML.Light.Lexer ( XmlSource )

import Control.Monad

import Prelude hiding (readFile)

#if MIN_VERSION_utf8_string(1,0,0)
-- | Read the file as a packed ByteString and then apply utf8 decoder.
-- System.IO.readFile looks at the the current locale to choose a decoder,
-- but here we always want to use the utf8 decoder.

import qualified Data.ByteString as BS (readFile)
import Data.ByteString.UTF8 as UTF8 (toString)

readFile :: FilePath -> IO String
readFile path = BS.readFile path >>= return . UTF8.toString

{-
-- Equivalent:
import System.IO (withBinaryFile, IOMode(ReadMode), hSetEncoding, utf8, hGetContents)

readFile :: FilePath -> IO String
readFile path = withBinaryFile path ReadMode $ \ h -> hSetEncoding h utf8 >> hGetContents h
-}

#else
import qualified System.IO.UTF8 as UTF8 ( readFile )

-- | Use the UTF8 version of readfile from the old utf8-string package.
readFile :: FilePath -> IO String
readFile = UTF8.readFile
#endif

-- | 'parseFeedFromFile fp' reads in the contents of the file at @fp@;
-- the assumed encoding is UTF-8.
parseFeedFromFile :: FilePath -> IO Feed
parseFeedFromFile fp = do
  ls <- readFile fp
  case parseFeedString ls of
    Nothing -> fail ("parseFeedFromFile: not a well-formed XML content in: " ++ fp)
    Just f  -> return f

-- | 'parseFeedWithParser tries to parse the string @str@
-- as one of the feed formats. First as Atom, then RSS2 before
-- giving RSS1 a try. @Nothing@ is, rather unhelpfully, returned
-- as an indication of error.
parseFeedWithParser :: XmlSource s => (s -> Maybe Element) -> s -> Maybe Feed
parseFeedWithParser parser str =
  case parser str of
    Nothing -> Nothing
    Just e ->
      readAtom e `mplus`
      readRSS2 e `mplus`
      readRSS1 e `mplus`
      Just (XMLFeed e)


-- | 'parseFeedString str' tries to parse the string @str@ as
-- one of the feed formats. First as Atom, then RSS2 before
-- giving RSS1 a try. @Nothing@ is, rather unhelpfully, returned
-- as an indication of error.
parseFeedString :: String -> Maybe Feed
parseFeedString = parseFeedWithParser parseXMLDoc

-- | 'readRSS2 elt' tries to derive an RSS2.x, RSS-0.9x feed document
-- from the XML element @e@.
readRSS2 :: XML.Element -> Maybe Feed
readRSS2 e = fmap RSSFeed  $ RSS.elementToRSS e

-- | 'readRSS1 elt' tries to derive an RSS1.0 feed document
-- from the XML element @e@.
readRSS1 :: XML.Element -> Maybe Feed
readRSS1 e = fmap RSS1Feed $ RSS1.elementToFeed e

-- | 'readAtom elt' tries to derive an Atom feed document
-- from the XML element @e@.
readAtom :: XML.Element -> Maybe Feed
readAtom e = fmap AtomFeed $ Atom.elementFeed e

