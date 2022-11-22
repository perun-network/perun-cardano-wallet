{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Ledger.Address (PaymentPubKey, PaymentPubKeyHash, paymentPubKeyHash)
import Network.Wai
import Network.Wai.Handler.Warp
import Perun.Onchain (ChannelState)
import Plutus.Contract.Oracle (SignedMessage)
import Servant
import Wallet.Wallet

type WalletApi =
  "getAddress" :> Capture "id" Integer :> Get '[JSON] PaymentPubKey
    :<|> "sign" :> Capture "id" Integer :> ReqBody '[JSON] ChannelState :> Post '[JSON] (SignedMessage ChannelState)
    :<|> "getAddressHash" :> Capture "id" Integer :> Get '[JSON] PaymentPubKeyHash

walletApi :: Proxy WalletApi
walletApi = Proxy

server :: Server WalletApi
server =
  getAddressHandler
    :<|> signHandler
    :<|> getAddressHashHandler
  where
    getAddressHandler :: Integer -> Handler PaymentPubKey
    getAddressHandler wId = return $ getPaymentPubKey (wallets !! fromIntegral wId)

    signHandler :: Integer -> ChannelState -> Handler (SignedMessage ChannelState)
    signHandler wId state = return $ signState state (wallets !! fromIntegral wId)

    getAddressHashHandler :: Integer -> Handler PaymentPubKeyHash
    getAddressHashHandler wId = return . paymentPubKeyHash $ getPaymentPubKey (wallets !! fromIntegral wId)

main :: IO ()
main = do
  let port = 8888
      settings =
        setPort port $
          setBeforeMainLoop
            (putStrLn ("perun-cardano-wallet: listening in port " ++ show port))
            defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve walletApi server

wallets :: [Wallet]
wallets = zipWith unsafeGenerateWalletFromInteger [0 .. 5] [0 .. 5]