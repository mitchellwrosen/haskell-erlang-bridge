module Main where

import Erlang.Distribution
import Erlang.Distribution.Internal
import Erlang.Distribution.Instances

import Data.Serialize

import Test.QuickCheck
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain props

props :: TestTree
props = testGroup "Properties"
    [ distributionProps
    ]

distributionProps :: TestTree
distributionProps = testGroup "Erlang.Distribution"
    [ decodeEncodeProps ]

decodeEncodeProps :: TestTree
decodeEncodeProps = testGroup "decode . encode == id"
    [ QC.testProperty "Digest"             (prop_decode_encode_id :: Digest         -> Property)
    , QC.testProperty "HandshakeFlags"     (prop_decode_encode_id :: HandshakeFlags -> Property)
    , QC.testProperty "Protocol"           (prop_decode_encode_id :: Protocol       -> Property)
    , QC.testProperty "DistributionHeader" prop_dist_header_decode_encode_id
    ]

prop_decode_encode_id :: (Eq a, Show a, Arbitrary a, Serialize a) => a -> Property
prop_decode_encode_id x = decode (encode x) === Right x

-- DistributionHeader is a little special, because we lose information (is_long_atom) when
-- encoding if there are no cache refs to encode.
prop_dist_header_decode_encode_id :: DistributionHeader -> Property
prop_dist_header_decode_encode_id x@(DistributionHeader _ [])   = decode (encode x) === Right (DistributionHeader False [])
prop_dist_header_decode_encode_id x@(DistributionHeader _ refs) = decode (encode x) === Right x

