{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Ledger.Crypto (PubKey, Signature)
import Perun (Channel, ChannelState)
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

data ChannelStateSigningRequest = ChannelStateSigningRequest
  { csPubKey :: !PubKey,
    csState :: !ChannelState
  }

data ChannelStateVerificationRequest = ChannelStateVerificationRequest
  { cvSignature :: !Signature,
    cvPubKey :: !PubKey,
    cvState :: !ChannelState
  }

data CalculateChannelIDReqest = Channel

type KeyAvailabilityRequest = PubKey

$(deriveJSON defaultOptions ''SigningRequest)
$(deriveJSON defaultOptions ''VerificationRequest)
$(deriveJSON defaultOptions ''ChannelStateSigningRequest)
$(deriveJSON defaultOptions ''ChannelStateVerificationRequest)
$(deriveJSON defaultOptions ''CalculateChannelIDReqest)
