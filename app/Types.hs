{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Ledger.Crypto (PubKey, Signature)
import Plutus.V2.Ledger.Api (BuiltinByteString)

-- IMPORTANT:
-- Do not change the identifier names here! They are part of the API.
data SigningRequest = SigningRequest
  { sPubKey :: !PubKey,
    sMessage :: !BuiltinByteString
  }

data VerificationRequest = VerificationRequest
  { vSignature :: !Signature,
    vPubKey :: !PubKey,
    vMessage :: !BuiltinByteString
  }

type KeyAvailabilityRequest = PubKey

$(deriveJSON defaultOptions ''SigningRequest)
$(deriveJSON defaultOptions ''VerificationRequest)