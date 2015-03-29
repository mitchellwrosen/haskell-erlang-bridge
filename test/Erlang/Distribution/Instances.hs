module Erlang.Distribution.Instances where

import Erlang.Distribution
import Erlang.Distribution.Internal

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString           as BS
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary AtomCacheRef where
    arbitrary = AtomCacheRef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary Digest where
    arbitrary = Digest . BS.pack <$> forM [1..16] (const arbitrary)

instance Arbitrary DistributionHeader where
    arbitrary = DistributionHeader <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary HandshakeChallenge where
    arbitrary = HandshakeChallenge <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary HandshakeChallengeReply where
    arbitrary = HandshakeChallengeReply <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary HandshakeFlag where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary HandshakeName where
    arbitrary = HandshakeName <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary HandshakeStatus where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Protocol where
    arbitrary = pure TCP_IPV4 -- lol

deriving instance Generic AtomCacheRef
deriving instance Generic DistributionHeader

deriving instance Arbitrary Challenge
deriving instance Arbitrary HandshakeChallengeAck
deriving instance Arbitrary HandshakeFlags
deriving instance Arbitrary InCookie
deriving instance Arbitrary Node
deriving instance Arbitrary OutCookie
deriving instance Arbitrary Version

