module Data.Attoparsec.IP where

import Data.Attoparsec.Text (Parser, char, digit, hexadecimal)
import Data.Word (Word8, Word16)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Net.Types (IPv4, IPv6)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6


ipv4 :: Parser IPv4
ipv4 = do
  a <- octet
  void $ char '.'
  b <- octet
  void $ char '.'
  c <- octet
  void $ char '.'
  d <- octet
  pure (IPv4.fromOctets a b c d)
  where
    octet :: Parser Word8
    octet = do
      (a,b,c) <-
        let oneDigit = do
              n <- digit
              pure ('0','0',n)
            twoDigit = do
              n <- digit
              m <- digit
              pure ('0',n,m)
            threeDigit = do
              n <- digit
              m <- digit
              o <- digit
              pure (n,m,o)
        in  threeDigit <|> twoDigit <|> oneDigit
      let n :: Word8
          n = read (a:b:c:[])
      pure n


ipv6 :: Parser IPv6
ipv6 = do
  a <- chunk
  void $ char ':'
  b <- chunk
  void $ char ':'
  c <- chunk
  void $ char ':'
  d <- chunk
  void $ char ':'
  (e,f,g,h) <- do
    let anotherColon = do
          void $ char ':'
          pure (0,0,0,0)
        moreChunks = do
          e' <- chunk
          void $ char ':'
          f' <- chunk
          void $ char ':'
          g' <- chunk
          void $ char ':'
          h' <- chunk
          pure (e',f',g',h')
    anotherColon <|> moreChunks
  pure (IPv6.fromWord16s a b c d e f g h)
  where
    chunk :: Parser Word16
    chunk = do
      (a,b,c,d) <- (,,,) <$> hexadecimal <*> hexadecimal <*> hexadecimal <*> hexadecimal
      pure $ fromInteger $ a * (16^3)
                         + b * (16^2)
                         + c * 16
                         + d
