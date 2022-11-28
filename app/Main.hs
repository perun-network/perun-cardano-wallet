{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Ledger.Address (PaymentPubKey)
import Network.Wai
import Network.Wai.Handler.Warp
import Plutus.Contract.Oracle (SignedMessage)
import Plutus.V2.Ledger.Api (BuiltinByteString)
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
import Wallet.Wallet

type WalletApi =
  "sign" Servant.:> ReqBody '[JSON] PaymentPubKey Servant.:> ReqBody '[OctetStream] BuiltinByteString Servant.:> Post '[OctetStream] (SignedMessage BuiltinByteString)
    Servant.:<|> "exists" Servant.:> ReqBody '[JSON] PaymentPubKey Servant.:> Post '[JSON] Bool
    Servant.:<|> "verfiy" Servant.:> ReqBody '[JSON] PaymentPubKey Servant.:> ReqBody '[JSON] (SignedMessage BuiltinByteString) Servant.:> Post '[JSON] Bool

walletApi :: Servant.Proxy WalletApi
walletApi = Servant.Proxy

server :: Servant.Server WalletApi
server =
  signHandler
    Servant.:<|> existsHandler
    Servant.:<|> verifyHandler
  where
    signHandler :: PaymentPubKey -> BuiltinByteString -> Servant.Handler (SignedMessage BuiltinByteString)
    signHandler hash msg = do
      wallet <- case Map.lookup hash walletMap of
        Nothing -> throwError walletNotFoundError
        Just w -> return w
      return $ signData msg wallet

    existsHandler :: PaymentPubKey -> Servant.Handler Bool
    existsHandler hash = return $ Map.member hash walletMap

    verifyHandler :: PaymentPubKey -> SignedMessage BuiltinByteString -> Handler Bool
    verifyHandler hash = undefined

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

walletMap :: Map.Map PaymentPubKey Wallet
walletMap = Map.fromList $ map (\x -> (getPaymentPubKey x, x)) wallets

walletNotFoundError :: ServerError
walletNotFoundError = err404 {errBody = "No Wallet found for given PaymentPubKeyHash"}