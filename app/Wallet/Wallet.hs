module Wallet.Wallet where

import qualified Cardano.Crypto.Wallet as Crypto
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Ledger (Datum (..), PaymentPubKey (PaymentPubKey), PubKey)
import Ledger.Crypto (signedBy, toPublicKey)
import qualified Ledger.Crypto as Crypto (Signature, generateFromSeed', sign')
import qualified Ledger.Scripts as S
import Perun (ChannelState)
import Plutus.Contract.Oracle (SignedMessage (..), signMessage', verifySignedMessageOffChain)
import Plutus.V2.Ledger.Api (BuiltinByteString, ToData (toBuiltinData))

data Wallet = Wallet
  { privateKey :: !Crypto.XPrv
  }

signChannelState :: ChannelState -> Wallet -> Crypto.Signature
signChannelState state (Wallet key) =
  let sm = signMessage' state key
   in osmSignature sm

signData :: BuiltinByteString -> Wallet -> Crypto.Signature
signData msg (Wallet key) = Crypto.sign' msg key

verifySignature :: Crypto.Signature -> PubKey -> BuiltinByteString -> Bool
verifySignature = signedBy

verifyChannelStateSignature :: Crypto.Signature -> PubKey -> ChannelState -> Bool
verifyChannelStateSignature sig key state =
  let datum = Datum $ toBuiltinData state
      datumHash = S.datumHash datum
      sm = SignedMessage sig datumHash datum :: SignedMessage ChannelState
      v = verifySignedMessageOffChain (PaymentPubKey key) sm
   in case v of
        Left _ -> False
        Right _ -> True

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