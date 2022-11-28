module Wallet.Wallet where

import qualified Cardano.Crypto.Wallet as Crypto
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Ledger.Address (PaymentPubKey (PaymentPubKey))
import Ledger.Crypto (toPublicKey)
import qualified Ledger.Crypto as Crypto (generateFromSeed')
import Plutus.Contract.Oracle (SignedMessage, signMessage')
import Plutus.V2.Ledger.Api (BuiltinByteString)

data Wallet = Wallet
  { privateKey :: !Crypto.XPrv
  }

signData :: BuiltinByteString -> Wallet -> SignedMessage BuiltinByteString
signData msg (Wallet key) = signMessage' msg key

getPaymentPubKey :: Wallet -> PaymentPubKey
getPaymentPubKey (Wallet key) = PaymentPubKey $ toPublicKey key

unsafeGenerateFromInteger :: Integer -> Crypto.XPrv
unsafeGenerateFromInteger seed =
  let seedString = BSL.toStrict $ serialise seed
      missing = max 0 (32 - BS.length seedString)
      seedStringPadded = seedString <> BS.replicate missing 0
   in Crypto.generateFromSeed' seedStringPadded

unsafeGenerateWalletFromInteger :: Integer -> Wallet
unsafeGenerateWalletFromInteger seed = Wallet (unsafeGenerateFromInteger seed)