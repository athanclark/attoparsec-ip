{-# LANGUAGE
    OverloadedLists
  , OverloadedStrings
  , RecordWildCards
  #-}

module Data.Attoparsec.IP (ipv4, ipv6) where

import Data.Attoparsec.Text (Parser) --, char, string, digit, hexadecimal, many1)
-- import Data.Word (Word8, Word16)
-- import Data.Vector (Vector)
-- import qualified Data.Vector as V
-- import Data.Monoid ((<>))
-- import Control.Applicative ((<|>))
-- import Control.Monad (void)
import Net.Types (IPv4, IPv6)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6

ipv4 :: Parser IPv4
ipv4 = IPv4.parser

ipv6 :: Parser IPv6
ipv6 = IPv6.parser

-- ipv4 :: Parser IPv4
-- ipv4 = do
--   a <- octet
--   void (char '.')
--   b <- octet
--   void (char '.')
--   c <- octet
--   void (char '.')
--   d <- octet
--   pure (IPv4.fromOctets a b c d)
--   where
--     octet :: Parser Word8
--     octet = do
--       (a,b,c) <-
--         let oneDigit = do
--               n <- digit
--               pure ('0','0',n)
--             twoDigit = do
--               n <- digit
--               m <- digit
--               pure ('0',n,m)
--             threeDigit = do
--               n <- digit
--               m <- digit
--               o <- digit
--               pure (n,m,o)
--         in  threeDigit <|> twoDigit <|> oneDigit
--       let n :: Word8
--           n = read (a:b:c:[])
--       pure n


-- data IPv6Divider
--   = DividerColon
--   | DoubleColon
--   deriving (Show)

-- parseDividerOrDoubleColon :: Parser IPv6Divider
-- parseDividerOrDoubleColon =
--   let divider = DividerColon <$ char ':'
--       doubleColon = DoubleColon <$ string "::"
--   in  doubleColon <|> divider

-- parseHextet :: Parser IPv6Chunk
-- parseHextet = Hextet <$> hexadecimal

-- parseHextets :: Parser [IPv6Chunk]
-- parseHextets = many1 (parseHextet <|> (Divider <$> parseDividerOrDoubleColon))

-- data IPv6Chunk
--   = Hextet Word16
--   | Divider IPv6Divider
--   deriving (Show)

-- data IPv6TokenPos
--   = Init
--   | A
--   | B
--   | C
--   | D
--   | E
--   | F
--   | G
--   | Finished
--   deriving (Show, Eq, Ord, Enum, Bounded)

-- data IPv6State = IPv6State
--   { hextets :: Vector Word16
--   , current :: IPv6TokenPos
--   , doublePos :: Maybe IPv6TokenPos
--   } deriving (Show)

-- initIPv6State :: IPv6State
-- initIPv6State = IPv6State [] Init Nothing

-- accumIPv6State :: [IPv6Chunk] -> IPv6State
-- accumIPv6State xs =
--   let go :: IPv6State -> IPv6Chunk -> IPv6State
--       go xss@IPv6State{..} x = case x of
--         Divider d -> case d of
--           DoubleColon -> xss { doublePos = Just current }
--           DividerColon -> xss
--         Hextet n -> xss { hextets = V.snoc hextets n, current = succ current }
--   in  foldl go initIPv6State xs

-- ipv6StateToIPv6 :: IPv6State -> Maybe IPv6
-- ipv6StateToIPv6 IPv6State{..} = case doublePos of
--   Nothing -> case V.toList hextets of
--     (a:b:c:d:e:f:g:h:_) -> Just (IPv6.fromWord16s a b c d e f g h)
--     _ -> Nothing
--   Just p ->
--     let zeros = V.replicate (8 - V.length hextets) 0
--         composite = case p of
--           Init -> Just (zeros <> hextets)
--           A -> case V.toList hextets of
--             (a:hs) -> Just ([a] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           B -> case V.toList hextets of
--             (a:b:hs) -> Just ([a,b] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           C -> case V.toList hextets of
--             (a:b:c:hs) -> Just ([a,b,c] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           D -> case V.toList hextets of
--             (a:b:c:d:hs) -> Just ([a,b,c,d] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           E -> case V.toList hextets of
--             (a:b:c:d:e:hs) -> Just ([a,b,c,d,e] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           F -> case V.toList hextets of
--             (a:b:c:d:e:f:hs) -> Just ([a,b,c,d,e,f] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           G -> case V.toList hextets of
--             (a:b:c:d:e:f:g:hs) -> Just ([a,b,c,d,e,f,g] <> zeros <> V.fromList hs)
--             _ -> Nothing
--           Finished -> case V.toList hextets of
--             (a:b:c:d:e:f:g:h:_) -> Just [a,b,c,d,e,f,g,h]
--             _ -> Nothing
--     in  case V.toList <$> composite of
--       Just (a:b:c:d:e:f:g:h:_) -> Just (IPv6.fromWord16s a b c d e f g h)
--       _ -> Nothing

-- ipv6 :: Parser IPv6
-- ipv6 = do
--   s <- parseHextets
--   case toIPv6 s of
--     Nothing -> fail "Not an IPv6"
--     Just x -> pure x
--   where
--     toIPv6 :: [IPv6Chunk] -> Maybe IPv6
--     toIPv6 = ipv6StateToIPv6 . accumIPv6State
