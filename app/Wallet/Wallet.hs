{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Wallet.Wallet where

import qualified Cardano.Crypto.Wallet as Crypto
import Data.Text
import Data.Text.Encoding
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Ledger (Datum (..), PaymentPubKey (PaymentPubKey), PubKey, Passphrase, PaymentPrivateKey(..))
import Ledger.Crypto (signedBy, toPublicKey)
import qualified Ledger.Crypto as Crypto (Signature, generateFromSeed, generateFromSeed', sign', Passphrase (..))
import qualified Ledger.Scripts as S
import Perun (ChannelState)
import Plutus.Contract.Oracle (SignedMessage (..), signMessage, verifySignedMessageOffChain, SignedMessageCheckError)
import Plutus.V2.Ledger.Api (BuiltinByteString, ToData (toBuiltinData))
import Data.Aeson
import GHC.Generics

data Wallet = Wallet
  { privateKey :: !Crypto.XPrv,
    passPhrase :: !Crypto.Passphrase
  }

data WalletGenerationException = WalletGenerationException
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  
signChannelState :: ChannelState -> Wallet -> Crypto.Signature
signChannelState state (Wallet key p) =
  let sm = signMessage state (PaymentPrivateKey key) p
   in osmSignature sm

signData :: BuiltinByteString -> Wallet -> Crypto.Signature
signData msg (Wallet key _) = Crypto.sign' msg key

verifySignature :: Crypto.Signature -> PubKey -> BuiltinByteString -> Bool
verifySignature = signedBy

verifyChannelStateSignature :: Crypto.Signature -> PubKey -> ChannelState -> Either SignedMessageCheckError Bool
verifyChannelStateSignature sig key state =
  let datum = Datum $ toBuiltinData state
      datumHash = S.datumHash datum
      sm = SignedMessage sig datumHash datum :: SignedMessage ChannelState
      v = verifySignedMessageOffChain (PaymentPubKey key) sm
   in case v of
        Left e -> Left e
        Right cs -> Right (cs == state)

getPubKey :: Wallet -> PubKey
getPubKey (Wallet key _) = toPublicKey key

-- unsafeGenerateFromInteger :: Integer -> Crypto.XPrv
-- unsafeGenerateFromInteger seed =
--   let seedString = BSL.toStrict $ serialise seed
--       missing = max 0 (32 - BS.length seedString)
--       seedStringPadded = seedString <> BS.replicate missing 0
--    in Crypto.generateFromSeed' seedStringPadded
-- 
-- unsafeGenerateWalletFromInteger :: Integer -> Wallet
-- unsafeGenerateWalletFromInteger seed = Wallet (unsafeGenerateFromInteger seed)

aliceWallet :: Wallet
aliceWallet = Wallet (unPaymentPrivateKey $ privateKeyFromMnemonic (alicePhrase, alicePassphrase)) alicePassphrase

bobWallet :: Wallet
bobWallet = Wallet (unPaymentPrivateKey $ privateKeyFromMnemonic (bobPhrase, bobPassphrase)) bobPassphrase

privateKeyFromMnemonic :: ([Text], Passphrase) -> PaymentPrivateKey
privateKeyFromMnemonic (mnemonic, passphrase) =
  PaymentPrivateKey . Crypto.generateFromSeed (BS.concat $ Prelude.map encodeUtf8 mnemonic) $
    passphrase

alicePhrase :: [Text]
alicePhrase =
  [ "lottery",
    "ring",
    "detect",
    "drip",
    "black",
    "match",
    "spoon",
    "post",
    "bind",
    "suit",
    "gather",
    "someone",
    "notice",
    "hero",
    "scrap"
  ]

bobPhrase :: [Text]
bobPhrase =
  [ "drill",
    "piece",
    "rotate",
    "badge",
    "rapid",
    "foam",
    "ginger",
    "panda",
    "velvet",
    "version",
    "duck",
    "travel",
    "script",
    "police",
    "enemy"
  ]

alicePassphrase :: Crypto.Passphrase
alicePassphrase = Crypto.Passphrase "0123456789"

bobPassphrase :: Crypto.Passphrase
bobPassphrase = Crypto.Passphrase "0123456789"
