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
    Servant.:<|> "exists" Servant.:> ReqBody '[JSON] PubKey Servant.:> Post '[JSON] Bool
    Servant.:<|> "verfiy" Servant.:> ReqBody '[JSON] VerificationRequest Servant.:> Post '[JSON] Bool

walletApi :: Servant.Proxy WalletApi
walletApi = Servant.Proxy

server :: Servant.Server WalletApi
server =
  signHandler
    Servant.:<|> existsHandler
    Servant.:<|> verifyHandler
  where
    signHandler :: SigningRequest -> Servant.Handler Signature
    signHandler (SigningRequest publicKey msg) = do
      wallet <- case Map.lookup publicKey walletMap of
        Nothing -> throwError walletNotFoundError
        Just w -> return w
      return $ signData msg wallet

    existsHandler :: PubKey -> Servant.Handler Bool
    existsHandler key = return $ Map.member key walletMap

    verifyHandler :: VerificationRequest -> Handler Bool
    verifyHandler (VerificationRequest signature publicKey message) = return $ verifySignature signature publicKey message

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
