module Main where

import Erlang.Distribution
import Erlang.Distribution.Internal

import Data.Serialize

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain props

props :: TestTree
props = testGroup "Properties"
    [ handshakeProps
    ]

handshakeProps :: TestTree
handshakeProps = testGroup "Handshake properties"
    [ QC.testProperty "HandshakeName decode-encode identity" $
          \(handshake_name :: HandshakeName) -> decode (encode handshake_name) == Right handshake_name
    ]
