{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Customer (
  Customer()
  , HasCustomer, theCustomerMaybe, setCustomer, theCustomer, HasCustomers, theCustomersMaybe, setCustomers, theCustomers
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.CustomerState
import Haskify.Data.CustomerAddress

data Customer = Customer
 { accepts_merketing    :: Maybe Bool
 , addresses            :: Maybe [CustomerAddress]
 , created_at           :: Maybe DateTime
 , default_address      :: Maybe CustomerAddress
 , email                :: Maybe Text
 , first_name           :: Maybe Text
 , id                   :: Maybe IDNumber
 , multipass_identifier :: Maybe Text
 , last_name            :: Maybe Text
 , last_order_id        :: Maybe IDNumber
 , last_order_name      :: Maybe Text
 , note                 :: Maybe Text
 , orders_count         :: Maybe Int
 , state                :: Maybe CustomerState
 , tags                 :: Maybe Text
 , tax_exempt           :: Maybe Bool
 , total_spent          :: Maybe Text
 , updated_at           :: Maybe DateTime
 , verified_email       :: Maybe Bool
 } deriving (Eq, Show)

class HasCustomer t where
  theCustomerMaybe :: t -> Maybe Customer
  setCustomer :: Customer -> t -> t

  theCustomer :: t -> HaskifyM Customer
  theCustomer x = case theCustomerMaybe x of
    Nothing -> fieldDNE "Customer"
    Just y -> return y

class HasCustomers t where
  theCustomersMaybe :: t -> Maybe [Customer]
  setCustomers :: [Customer] -> t -> t

  theCustomers :: t -> HaskifyM [Customer]
  theCustomers x = case theCustomersMaybe x of
    Nothing -> fieldDNE "Customer"
    Just y -> return y

instance NullObject Customer where
  nullObject = Customer
    { accepts_merketing    = Nothing
    , addresses            = Nothing
    , created_at           = Nothing
    , default_address      = Nothing
    , email                = Nothing
    , first_name           = Nothing
    , id                   = Nothing
    , multipass_identifier = Nothing
    , last_name            = Nothing
    , last_order_id        = Nothing
    , last_order_name      = Nothing
    , note                 = Nothing
    , orders_count         = Nothing
    , state                = Nothing
    , tags                 = Nothing
    , tax_exempt           = Nothing
    , total_spent          = Nothing
    , updated_at           = Nothing
    , verified_email       = Nothing
   }

instance FromJSON Customer where
  parseJSON = withObject "Customer" $ \o -> do
    accepts_merketing    <- o .:? "accepts_merketing"
    addresses            <- o .:? "addresses"
    created_at           <- o .:? "created_at"
    default_address      <- o .:? "default_address"
    email                <- o .:? "email"
    first_name           <- o .:? "first_name"
    id                   <- o .:? "id"
    multipass_identifier <- o .:? "multipass_identifier"
    last_name            <- o .:? "last_name"
    last_order_id        <- o .:? "last_order_id"
    last_order_name      <- o .:? "last_order_name"
    note                 <- o .:? "note"
    orders_count         <- o .:? "orders_count"
    state                <- o .:? "state"
    tags                 <- o .:? "tags"
    tax_exempt           <- o .:? "tax_exempt"
    total_spent          <- o .:? "total_spent"
    updated_at           <- o .:? "updated_at"
    verified_email       <- o .:? "verified_email"
    return Customer{..}


instance ToJSON Customer where
  toJSON Customer{..} = objectSansNull
    [ "accepts_merketing"    .= accepts_merketing
    , "addresses"            .= addresses
    , "created_at"           .= created_at
    , "default_address"      .= default_address
    , "email"                .= email
    , "first_name"           .= first_name
    , "id"                   .= id
    , "multipass_identifier" .= multipass_identifier
    , "last_name"            .= last_name
    , "last_order_id"        .= last_order_id
    , "last_order_name"      .= last_order_name
    , "note"                 .= note
    , "orders_count"         .= orders_count
    , "state"                .= state
    , "tags"                 .= tags
    , "tax_exempt"           .= tax_exempt
    , "total_spent"          .= total_spent
    , "updated_at"           .= updated_at
    , "verified_email"       .= verified_email
    ]


instance ToURLArgs Customer where
  toURLArgs Customer{..} = intercalate "&" $ filter (/= "")
    [ case accepts_merketing of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[accepts_merketing]=", pack $ show x]
    , case addresses of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[addresses]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[created_at]=", pack $ show x]
    , case default_address of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[default_address]=", pack $ show x]
    , case email of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[email]=", pack $ show x]
    , case first_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[first_name]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[id]=", pack $ show x]
    , case multipass_identifier of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[multipass_identifier]=", pack $ show x]
    , case last_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[last_name]=", pack $ show x]
    , case last_order_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[last_order_id]=", pack $ show x]
    , case last_order_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[last_order_name]=", pack $ show x]
    , case note of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[note]=", pack $ show x]
    , case orders_count of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[orders_count]=", pack $ show x]
    , case state of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[state]=", pack $ show x]
    , case tags of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[tags]=", pack $ show x]
    , case tax_exempt of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[tax_exempt]=", pack $ show x]
    , case total_spent of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[total_spent]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[updated_at]=", pack $ show x]
    , case verified_email of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer[verified_email]=", pack $ show x]
    ]

instance HasAcceptsMarketing Customer where
  theAcceptsMarketingMaybe = accepts_merketing    
  setAcceptsMarketing x y = y { accepts_merketing = Just x }

instance HasCustomerAddresss Customer where
  theCustomerAddresssMaybe = addresses            
  setCustomerAddresss x y = y { addresses = Just x }

instance HasCreatedAt Customer where
  theCreatedAtMaybe = created_at           
  setCreatedAt x y = y { created_at = Just x }

instance HasDefaultAddress Customer where
  theDefaultAddressMaybe = default_address      
  setDefaultAddress x y = y { default_address = Just x }

instance HasEmail Customer where
  theEmailMaybe = email                
  setEmail x y = y { email = Just x }

instance HasFirstName Customer where
  theFirstNameMaybe = first_name           
  setFirstName x y = y { first_name = Just x }

instance HasID Customer where
  theIDMaybe = id                   
  setID x y = y { id = Just x }

instance HasMultipassIdentifier Customer where
  theMultipassIdentifierMaybe = multipass_identifier 
  setMultipassIdentifier x y = y { multipass_identifier = Just x }

instance HasLastName Customer where
  theLastNameMaybe = last_name            
  setLastName x y = y { last_name = Just x }

instance HasLastOrderID Customer where
  theLastOrderIDMaybe = last_order_id        
  setLastOrderID x y = y { last_order_id = Just x }

instance HasLastOrderName Customer where
  theLastOrderNameMaybe = last_order_name      
  setLastOrderName x y = y { last_order_name = Just x }

instance HasNote Customer where
  theNoteMaybe = note                 
  setNote x y = y { note = Just x }

instance HasOrdersCount Customer where
  theOrdersCountMaybe = orders_count         
  setOrdersCount x y = y { orders_count = Just x }

instance HasCustomerState Customer where
  theCustomerStateMaybe = state                
  setCustomerState x y = y { state = Just x }

instance HasTags Customer where
  theTagsMaybe = tags                 
  setTags x y = y { tags = Just x }

instance HasTaxExempt Customer where
  theTaxExemptMaybe = tax_exempt           
  setTaxExempt x y = y { tax_exempt = Just x }

instance HasTotalSpent Customer where
  theTotalSpentMaybe = total_spent          
  setTotalSpent x y = y { total_spent = Just x }

instance HasUpdatedAt Customer where
  theUpdatedAtMaybe = updated_at           
  setUpdatedAt x y = y { updated_at = Just x }

instance HasVerifiedEmail Customer where
  theVerifiedEmailMaybe = verified_email       
  setVerifiedEmail x y = y { verified_email = Just x }

