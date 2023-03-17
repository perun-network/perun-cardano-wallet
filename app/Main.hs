{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Ledger (PubKey)
import Ledger.Crypto (Signature)
import qualified Ledger.Scripts as S
import Network.Wai
import Network.Wai.Handler.Warp
import Perun.Onchain (Channel (..), ChannelID (..))
import Perun.Offchain (getChannelId)
import Plutus.V2.Ledger.Api (BuiltinByteString, toBuiltinData)
import Servant
  ( Handler,
    Proxy (..),
    Server,
    ServerError (..),
    err404,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.API
import Types
import Wallet.Wallet
import Control.Logger.Simple
import Data.Text (pack)
import Data.Coerce (coerce)

type WalletApi =
  "sign" Servant.:> ReqBody '[JSON] SigningRequest Servant.:> Post '[JSON] Signature
    Servant.:<|> "keyAvailable" Servant.:> ReqBody '[JSON] KeyAvailabilityRequest Servant.:> Post '[JSON] Bool
    Servant.:<|> "verify" Servant.:> ReqBody '[JSON] VerificationRequest Servant.:> Post '[JSON] Bool
    Servant.:<|> "signChannelState" Servant.:> ReqBody '[JSON] ChannelStateSigningRequest Servant.:> Post '[JSON] Signature
    Servant.:<|> "verifyChannelState" Servant.:> ReqBody '[JSON] ChannelStateVerificationRequest Servant.:> Post '[JSON] Bool
    Servant.:<|> "calculateChannelID" Servant.:> ReqBody '[JSON] Channel Servant.:> Post '[JSON] BuiltinByteString

walletApi :: Servant.Proxy WalletApi
walletApi = Servant.Proxy

server :: Servant.Server WalletApi
server =
  signHandler
    Servant.:<|> keyAvailableHandler
    Servant.:<|> verifyHandler
    Servant.:<|> signChannelStateHandler
    Servant.:<|> verifyChannelState
    Servant.:<|> calculateChannelID
  where
    signHandler :: SigningRequest -> Servant.Handler Signature
    signHandler (SigningRequest publicKey msg) = do
      logInfo . pack . unwords $ ["Signing message for", show publicKey]
      wallet <- case Map.lookup publicKey walletMap of
        Nothing -> do 
          logError . pack $ show walletNotFoundError
          throwError walletNotFoundError
        Just w -> return w
      return $ signData msg wallet

    keyAvailableHandler :: KeyAvailabilityRequest -> Servant.Handler Bool
    keyAvailableHandler key = do
      logInfo . pack . unwords $ ["Checking key-availability for", show key, "result:", show $ Map.member key walletMap]
      return $ Map.member key walletMap

    verifyHandler :: VerificationRequest -> Handler Bool
    verifyHandler (VerificationRequest signature publicKey message) = do
      logInfo . pack . unwords $ ["Verifying Signature for key", show publicKey, "result:"]
      return $ verifySignature signature publicKey message

    signChannelStateHandler :: ChannelStateSigningRequest -> Servant.Handler Signature
    signChannelStateHandler (ChannelStateSigningRequest publicKey state) = do
      logInfo . pack . unwords $ ["Signing ChannelState", show state,  "for", show publicKey]
      wallet <- case Map.lookup publicKey walletMap of
        Nothing -> do 
          logError . pack $ show walletNotFoundError
          throwError walletNotFoundError
        Just w -> return w
      let sig = signChannelState state wallet
      logInfo . pack . unwords $ ["Signature:", show sig]
      return sig

    verifyChannelState :: ChannelStateVerificationRequest -> Handler Bool
    verifyChannelState (ChannelStateVerificationRequest signature publicKey state) = do
      let res = verifyChannelStateSignature signature publicKey state
      logInfo . pack . unwords $ ["Verifying ChannelState Signature for key", show publicKey, "and Signature:", show signature, "result:", show res]
      case res of
        Left _ -> return False
        Right x -> return x

    calculateChannelID :: Channel -> Handler BuiltinByteString
    calculateChannelID ch = do
      let x@(ChannelID cid) = getChannelId ch 
      logInfo . pack . unwords $ ["Calculating ChannelID for", show ch, "result:", show x]
      return cid

main :: IO ()
main = do
  withGlobalLogging (LogConfig Nothing True) $ do
    let port = 8888
        settings =
          setPort port $
            setBeforeMainLoop
              (putStrLn ("perun-cardano-wallet: listening in port " ++ show port ++ " ..."))
              defaultSettings
    runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ Servant.serve walletApi server

wallets :: [Wallet]
wallets = [aliceWallet, bobWallet]

walletMap :: Map.Map PubKey Wallet
walletMap = Map.fromList $ map (\x -> (getPubKey x, x)) wallets

walletNotFoundError :: ServerError
walletNotFoundError = err404 {errBody = "No Wallet found for given PubKey"}
