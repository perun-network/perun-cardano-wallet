module Wallet.Wallet where

import qualified Cardano.Crypto.Wallet as Crypto
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Ledger (PubKey, signedBy)
import Ledger.Crypto (toPublicKey)
import qualified Ledger.Crypto as Crypto (Signature, generateFromSeed', sign')
import Plutus.V2.Ledger.Api (BuiltinByteString)

data Wallet = Wallet
  { privateKey :: !Crypto.XPrv
  }

signData :: BuiltinByteString -> Wallet -> Crypto.Signature
signData msg (Wallet key) = Crypto.sign' msg key

verifySignature :: Crypto.Signature -> PubKey -> BuiltinByteString -> Bool
verifySignature = signedBy

getPubKey :: Wallet -> PubKey
getPubKey (Wallet key) = toPublicKey key

unsafeGenerateFromInteger :: Integer -> Crypto.XPrv
unsafeGenerateFromInteger seed =
  let seedString = BSL.toStrict $ serialise seed
      missing = max 0 (32 - BS.length seedString)
      seedStringPadded = seedString <> BS.replicate missing 0
   in Crypto.generateFromSeed' seedStringPadded

unsafeGenerateWalletFromInteger :: Integer -> Wallet
unsafeGenerateWalletFromInteger seed = Wallet (unsafeGenerateFromInteger seed)