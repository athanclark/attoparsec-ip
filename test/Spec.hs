import Data.Attoparsec.IP (ipv4, ipv6)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6

import Data.Text (Text)
import Data.Attoparsec.Text (Parser, parseOnly)
import Test.Tasty (testGroup, defaultMain)
import Test.QuickCheck (Arbitrary (..))
import qualified Test.Tasty.QuickCheck as Q
import Test.QuickCheck.Property (Result, succeeded, failed)


instance Arbitrary IPv4.IPv4 where
  arbitrary = IPv4.ipv4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary IPv6.IPv6 where
  arbitrary = IPv6.ipv6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary



main :: IO ()
main = defaultMain $ testGroup "IP tests"
  [ Q.testProperty "IPv4" (parsePrintIso IPv4.encode ipv4)
  , Q.testProperty "IPv6" (parsePrintIso IPv6.encode ipv6)
  ]


parsePrintIso :: Eq a => (a -> Text) -> Parser a -> a -> Result
parsePrintIso print parser x = case parseOnly parser (print x) of
  Left e -> failed
  Right y
    | y == x -> succeeded
    | otherwise -> failed
