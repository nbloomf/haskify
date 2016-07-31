{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Transaction (
  Transaction()
  , HasTransaction, theTransactionMaybe, setTransaction, theTransaction, HasTransactions, theTransactionsMaybe, setTransactions, theTransactions
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.PaymentDetails
import Haskify.Value.TransactionErrorCode
import Haskify.Value.TransactionReceipt
import Haskify.Value.TransactionStatus
import Haskify.Value.TransactionKind

data Transaction = Transaction
 { amount          :: Maybe Text
 , authorization   :: Maybe Text
 , created_at      :: Maybe DateTime
 , device_id       :: Maybe IDNumber
 , gateway         :: Maybe Text
 , source_name     :: Maybe Text
 , payment_details :: Maybe PaymentDetails
 , id              :: Maybe IDNumber
 , kind            :: Maybe TransactionKind
 , order_id        :: Maybe IDNumber
 , receipt         :: Maybe TransactionReceipt
 , error_code      :: Maybe TransactionErrorCode
 , status          :: Maybe TransactionStatus
 , test            :: Maybe Bool
 , user_id         :: Maybe IDNumber
 , currency        :: Maybe Text
 } deriving (Eq, Show)

class HasTransaction t where
  theTransactionMaybe :: t -> Maybe Transaction
  setTransaction :: Transaction -> t -> t

  theTransaction :: t -> HaskifyM Transaction
  theTransaction x = case theTransactionMaybe x of
    Nothing -> fieldDNE "Transaction"
    Just y -> return y

class HasTransactions t where
  theTransactionsMaybe :: t -> Maybe [Transaction]
  setTransactions :: [Transaction] -> t -> t

  theTransactions :: t -> HaskifyM [Transaction]
  theTransactions x = case theTransactionsMaybe x of
    Nothing -> fieldDNE "Transaction"
    Just y -> return y

instance NullObject Transaction where
  nullObject = Transaction
    { amount          = Nothing
    , authorization   = Nothing
    , created_at      = Nothing
    , device_id       = Nothing
    , gateway         = Nothing
    , source_name     = Nothing
    , payment_details = Nothing
    , id              = Nothing
    , kind            = Nothing
    , order_id        = Nothing
    , receipt         = Nothing
    , error_code      = Nothing
    , status          = Nothing
    , test            = Nothing
    , user_id         = Nothing
    , currency        = Nothing
   }

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    amount          <- o .:? "amount"
    authorization   <- o .:? "authorization"
    created_at      <- o .:? "created_at"
    device_id       <- o .:? "device_id"
    gateway         <- o .:? "gateway"
    source_name     <- o .:? "source_name"
    payment_details <- o .:? "payment_details"
    id              <- o .:? "id"
    kind            <- o .:? "kind"
    order_id        <- o .:? "order_id"
    receipt         <- o .:? "receipt"
    error_code      <- o .:? "error_code"
    status          <- o .:? "status"
    test            <- o .:? "test"
    user_id         <- o .:? "user_id"
    currency        <- o .:? "currency"
    return Transaction{..}


instance ToJSON Transaction where
  toJSON Transaction{..} = objectSansNull
    [ "amount"          .= amount
    , "authorization"   .= authorization
    , "created_at"      .= created_at
    , "device_id"       .= device_id
    , "gateway"         .= gateway
    , "source_name"     .= source_name
    , "payment_details" .= payment_details
    , "id"              .= id
    , "kind"            .= kind
    , "order_id"        .= order_id
    , "receipt"         .= receipt
    , "error_code"      .= error_code
    , "status"          .= status
    , "test"            .= test
    , "user_id"         .= user_id
    , "currency"        .= currency
    ]


instance ToURLArgs Transaction where
  toURLArgs Transaction{..} = intercalate "&" $ filter (/= "")
    [ case amount of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[amount]=", pack $ show x]
    , case authorization of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[authorization]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[created_at]=", pack $ show x]
    , case device_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[device_id]=", pack $ show x]
    , case gateway of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[gateway]=", pack $ show x]
    , case source_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[source_name]=", pack $ show x]
    , case payment_details of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[payment_details]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[id]=", pack $ show x]
    , case kind of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[kind]=", pack $ show x]
    , case order_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[order_id]=", pack $ show x]
    , case receipt of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[receipt]=", pack $ show x]
    , case error_code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[error_code]=", pack $ show x]
    , case status of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[status]=", pack $ show x]
    , case test of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[test]=", pack $ show x]
    , case user_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[user_id]=", pack $ show x]
    , case currency of
        Nothing -> ""
        Just x  -> Data.Text.concat ["transaction[currency]=", pack $ show x]
    ]

instance HasAmount Transaction where
  theAmountMaybe = amount          
  setAmount x y = y { amount = Just x }

instance HasAuthorization Transaction where
  theAuthorizationMaybe = authorization   
  setAuthorization x y = y { authorization = Just x }

instance HasCreatedAt Transaction where
  theCreatedAtMaybe = created_at      
  setCreatedAt x y = y { created_at = Just x }

instance HasDeviceID Transaction where
  theDeviceIDMaybe = device_id       
  setDeviceID x y = y { device_id = Just x }

instance HasGateway Transaction where
  theGatewayMaybe = gateway         
  setGateway x y = y { gateway = Just x }

instance HasSourceName Transaction where
  theSourceNameMaybe = source_name     
  setSourceName x y = y { source_name = Just x }

instance HasPaymentDetails Transaction where
  thePaymentDetailsMaybe = payment_details 
  setPaymentDetails x y = y { payment_details = Just x }

instance HasID Transaction where
  theIDMaybe = id              
  setID x y = y { id = Just x }

instance HasTransactionKind Transaction where
  theTransactionKindMaybe = kind            
  setTransactionKind x y = y { kind = Just x }

instance HasOrderID Transaction where
  theOrderIDMaybe = order_id        
  setOrderID x y = y { order_id = Just x }

instance HasTransactionReceipt Transaction where
  theTransactionReceiptMaybe = receipt         
  setTransactionReceipt x y = y { receipt = Just x }

instance HasTransactionErrorCode Transaction where
  theTransactionErrorCodeMaybe = error_code      
  setTransactionErrorCode x y = y { error_code = Just x }

instance HasTransactionStatus Transaction where
  theTransactionStatusMaybe = status          
  setTransactionStatus x y = y { status = Just x }

instance HasTest Transaction where
  theTestMaybe = test            
  setTest x y = y { test = Just x }

instance HasUserID Transaction where
  theUserIDMaybe = user_id         
  setUserID x y = y { user_id = Just x }

instance HasCurrency Transaction where
  theCurrencyMaybe = currency        
  setCurrency x y = y { currency = Just x }

