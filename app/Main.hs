{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map.Strict as Map
import Ledger.Address (PaymentPubKey, PaymentPubKeyHash, paymentPubKeyHash)
import Network.Wai
import Network.Wai.Handler.Warp
import Perun.Onchain (ChannelState)
import Plutus.Contract.Oracle (SignedMessage)
import Servant
  ( Handler,
    Proxy (..),
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.API
import Wallet.Wallet

type WalletApi =
  "getAddress" Servant.:> ReqBody '[JSON] PaymentPubKeyHash Servant.:> Post '[JSON] PaymentPubKey
    Servant.:<|> "sign" Servant.:> ReqBody '[JSON] PaymentPubKeyHash Servant.:> ReqBody '[JSON] ChannelState Servant.:> Post '[JSON] (SignedMessage ChannelState)
    Servant.:<|> "exists" Servant.:> ReqBody '[JSON] PaymentPubKeyHash Servant.:> Post '[JSON] Bool

walletApi :: Servant.Proxy WalletApi
walletApi = Servant.Proxy

server :: Servant.Server WalletApi
server =
  getAddressHandler
    Servant.:<|> signHandler
    Servant.:<|> existsHandler
  where
    getAddressHandler :: PaymentPubKeyHash -> Servant.Handler PaymentPubKey
    getAddressHandler hash = do
      return $
        ( \case
            Nothing -> error "No wallet found for given PaymentPubKeyHash"
            Just w -> getPaymentPubKey w
        )
          (Map.lookup hash walletMap)

    signHandler :: PaymentPubKeyHash -> ChannelState -> Servant.Handler (SignedMessage ChannelState)
    signHandler hash state = do
      let wallet =
            ( \case
                Nothing -> error "No wallet found for given PaymentPubKeyHash"
                Just w -> w
            )
              (Map.lookup hash walletMap)
      return $ signState state wallet

    existsHandler :: PaymentPubKeyHash -> Servant.Handler Bool
    existsHandler hash = return $ Map.member hash walletMap

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

walletMap :: Map.Map PaymentPubKeyHash Wallet
walletMap = Map.fromList $ map (\x -> (paymentPubKeyHash $ getPaymentPubKey x, x)) wallets