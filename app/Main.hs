{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Ledger (PubKey)
import Ledger.Crypto (Signature)
import Network.Wai
import Network.Wai.Handler.Warp
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

type WalletApi =
  "sign" Servant.:> ReqBody '[JSON] SigningRequest Servant.:> Post '[JSON] Signature
    Servant.:<|> "keyAvailable" Servant.:> ReqBody '[JSON] KeyAvailabilityRequest Servant.:> Post '[JSON] Bool
    Servant.:<|> "verify" Servant.:> ReqBody '[JSON] VerificationRequest Servant.:> Post '[JSON] Bool
    Servant.:<|> "signChannelState" Servant.:> ReqBody '[JSON] ChannelStateSigningRequest Servant.:> Post '[JSON] Signature
    Servant.:<|> "verifyChannelState" Servant.:> ReqBody '[JSON] ChannelStateVerificationRequest Servant.:> Post '[JSON] Bool

walletApi :: Servant.Proxy WalletApi
walletApi = Servant.Proxy

server :: Servant.Server WalletApi
server =
  signHandler
    Servant.:<|> keyAvailableHandler
    Servant.:<|> verifyHandler
    Servant.:<|> signChannelStateHandler
    Servant.:<|> verifyChannelState
  where
    signHandler :: SigningRequest -> Servant.Handler Signature
    signHandler (SigningRequest publicKey msg) = do
      wallet <- case Map.lookup publicKey walletMap of
        Nothing -> throwError walletNotFoundError
        Just w -> return w
      return $ signData msg wallet

    keyAvailableHandler :: KeyAvailabilityRequest -> Servant.Handler Bool
    keyAvailableHandler key = return $ Map.member key walletMap

    verifyHandler :: VerificationRequest -> Handler Bool
    verifyHandler (VerificationRequest signature publicKey message) = return $ verifySignature signature publicKey message

    signChannelStateHandler :: ChannelStateSigningRequest -> Servant.Handler Signature
    signChannelStateHandler (ChannelStateSigningRequest publicKey state) = do
      wallet <- case Map.lookup publicKey walletMap of
        Nothing -> throwError walletNotFoundError
        Just w -> return w
      return $ signChannelState state wallet

    verifyChannelState :: ChannelStateVerificationRequest -> Handler Bool
    verifyChannelState (ChannelStateVerificationRequest signature publicKey state) = return $ verifyChannelStateSignature signature publicKey state

main :: IO ()
main = do
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
wallets = unsafeGenerateWalletFromInteger <$> [0 .. 5]

walletMap :: Map.Map PubKey Wallet
walletMap = Map.fromList $ map (\x -> (getPubKey x, x)) wallets

walletNotFoundError :: ServerError
walletNotFoundError = err404 {errBody = "No Wallet found for given PubKey"}
