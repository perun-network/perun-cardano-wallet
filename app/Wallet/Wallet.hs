module Wallet.Wallet where

import qualified Cardano.Crypto.Wallet as Crypto
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Ledger.Address (PaymentPubKey (PaymentPubKey))
import Ledger.Crypto (toPublicKey)
import qualified Ledger.Crypto as Crypto (generateFromSeed')
import Perun (ChannelState)
import Plutus.Contract.Oracle (SignedMessage, signMessage')

data Wallet = Wallet
  { id :: !Integer,
    privateKey :: !Crypto.XPrv
  }

signState :: ChannelState -> Wallet -> SignedMessage ChannelState
signState state (Wallet _ key) = signMessage' state key

getPaymentPubKey :: Wallet -> PaymentPubKey
getPaymentPubKey (Wallet _ key) = PaymentPubKey $ toPublicKey key

unsafeGenerateFromInteger :: Integer -> Crypto.XPrv
unsafeGenerateFromInteger seed =
  let seedString = BSL.toStrict $ serialise seed
      missing = max 0 (32 - BS.length seedString)
      seedStringPadded = seedString <> BS.replicate missing 0
   in Crypto.generateFromSeed' seedStringPadded

unsafeGenerateWalletFromInteger :: Integer -> Integer -> Wallet
unsafeGenerateWalletFromInteger wId seed = Wallet wId (unsafeGenerateFromInteger seed)