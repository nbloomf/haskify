{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.ProductVariant (
  ProductVariant()
  , HasProductVariant, theProductVariantMaybe, setProductVariant, theProductVariant, HasProductVariants, theProductVariantsMaybe, setProductVariants, theProductVariants
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.InventoryManagement
import Haskify.Value.InventoryPolicy

data ProductVariant = ProductVariant
 { barcode                       :: Maybe Text
 , compare_at_price              :: Maybe Text
 , created_at                    :: Maybe DateTime
 , fulfillment_service           :: Maybe Text
 , grams                         :: Maybe Int
 , id                            :: Maybe IDNumber
 , image_id                      :: Maybe Int
 , inventory_management          :: Maybe InventoryManagement
 , inventory_policy              :: Maybe InventoryPolicy
 , inventory_quantity            :: Maybe Int
 , old_inventory_quantity        :: Maybe Int
 , inventory_quantity_adjustment :: Maybe Int
 , option1                       :: Maybe Text
 , option2                       :: Maybe Text
 , option3                       :: Maybe Text
 , position                      :: Maybe Int
 , price                         :: Maybe Text
 , product_id                    :: Maybe Int
 , requires_shipping             :: Maybe Bool
 , sku                           :: Maybe Text
 , taxable                       :: Maybe Bool
 , title                         :: Maybe Text
 , updated_at                    :: Maybe DateTime
 , weight                        :: Maybe Float
 , weight_unit                   :: Maybe Text
 } deriving (Eq, Show)

class HasProductVariant t where
  theProductVariantMaybe :: t -> Maybe ProductVariant
  setProductVariant :: ProductVariant -> t -> t

  theProductVariant :: t -> HaskifyM ProductVariant
  theProductVariant x = case theProductVariantMaybe x of
    Nothing -> fieldDNE "ProductVariant"
    Just y -> return y

class HasProductVariants t where
  theProductVariantsMaybe :: t -> Maybe [ProductVariant]
  setProductVariants :: [ProductVariant] -> t -> t

  theProductVariants :: t -> HaskifyM [ProductVariant]
  theProductVariants x = case theProductVariantsMaybe x of
    Nothing -> fieldDNE "ProductVariant"
    Just y -> return y

instance NullObject ProductVariant where
  nullObject = ProductVariant
    { barcode                       = Nothing
    , compare_at_price              = Nothing
    , created_at                    = Nothing
    , fulfillment_service           = Nothing
    , grams                         = Nothing
    , id                            = Nothing
    , image_id                      = Nothing
    , inventory_management          = Nothing
    , inventory_policy              = Nothing
    , inventory_quantity            = Nothing
    , old_inventory_quantity        = Nothing
    , inventory_quantity_adjustment = Nothing
    , option1                       = Nothing
    , option2                       = Nothing
    , option3                       = Nothing
    , position                      = Nothing
    , price                         = Nothing
    , product_id                    = Nothing
    , requires_shipping             = Nothing
    , sku                           = Nothing
    , taxable                       = Nothing
    , title                         = Nothing
    , updated_at                    = Nothing
    , weight                        = Nothing
    , weight_unit                   = Nothing
   }

instance FromJSON ProductVariant where
  parseJSON = withObject "ProductVariant" $ \o -> do
    barcode                       <- o .:? "barcode"
    compare_at_price              <- o .:? "compare_at_price"
    created_at                    <- o .:? "created_at"
    fulfillment_service           <- o .:? "fulfillment_service"
    grams                         <- o .:? "grams"
    id                            <- o .:? "id"
    image_id                      <- o .:? "image_id"
    inventory_management          <- o .:? "inventory_management"
    inventory_policy              <- o .:? "inventory_policy"
    inventory_quantity            <- o .:? "inventory_quantity"
    old_inventory_quantity        <- o .:? "old_inventory_quantity"
    inventory_quantity_adjustment <- o .:? "inventory_quantity_adjustment"
    option1                       <- o .:? "option1"
    option2                       <- o .:? "option2"
    option3                       <- o .:? "option3"
    position                      <- o .:? "position"
    price                         <- o .:? "price"
    product_id                    <- o .:? "product_id"
    requires_shipping             <- o .:? "requires_shipping"
    sku                           <- o .:? "sku"
    taxable                       <- o .:? "taxable"
    title                         <- o .:? "title"
    updated_at                    <- o .:? "updated_at"
    weight                        <- o .:? "weight"
    weight_unit                   <- o .:? "weight_unit"
    return ProductVariant{..}


instance ToJSON ProductVariant where
  toJSON ProductVariant{..} = objectSansNull
    [ "barcode"                       .= barcode
    , "compare_at_price"              .= compare_at_price
    , "created_at"                    .= created_at
    , "fulfillment_service"           .= fulfillment_service
    , "grams"                         .= grams
    , "id"                            .= id
    , "image_id"                      .= image_id
    , "inventory_management"          .= inventory_management
    , "inventory_policy"              .= inventory_policy
    , "inventory_quantity"            .= inventory_quantity
    , "old_inventory_quantity"        .= old_inventory_quantity
    , "inventory_quantity_adjustment" .= inventory_quantity_adjustment
    , "option1"                       .= option1
    , "option2"                       .= option2
    , "option3"                       .= option3
    , "position"                      .= position
    , "price"                         .= price
    , "product_id"                    .= product_id
    , "requires_shipping"             .= requires_shipping
    , "sku"                           .= sku
    , "taxable"                       .= taxable
    , "title"                         .= title
    , "updated_at"                    .= updated_at
    , "weight"                        .= weight
    , "weight_unit"                   .= weight_unit
    ]


instance ToURLArgs ProductVariant where
  toURLArgs ProductVariant{..} = intercalate "&" $ filter (/= "")
    [ case barcode of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[barcode]=", pack $ show x]
    , case compare_at_price of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[compare_at_price]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[created_at]=", pack $ show x]
    , case fulfillment_service of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[fulfillment_service]=", pack $ show x]
    , case grams of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[grams]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[id]=", pack $ show x]
    , case image_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[image_id]=", pack $ show x]
    , case inventory_management of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[inventory_management]=", pack $ show x]
    , case inventory_policy of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[inventory_policy]=", pack $ show x]
    , case inventory_quantity of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[inventory_quantity]=", pack $ show x]
    , case old_inventory_quantity of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[old_inventory_quantity]=", pack $ show x]
    , case inventory_quantity_adjustment of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[inventory_quantity_adjustment]=", pack $ show x]
    , case option1 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[option1]=", pack $ show x]
    , case option2 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[option2]=", pack $ show x]
    , case option3 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[option3]=", pack $ show x]
    , case position of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[position]=", pack $ show x]
    , case price of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[price]=", pack $ show x]
    , case product_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[product_id]=", pack $ show x]
    , case requires_shipping of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[requires_shipping]=", pack $ show x]
    , case sku of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[sku]=", pack $ show x]
    , case taxable of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[taxable]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[title]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[updated_at]=", pack $ show x]
    , case weight of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[weight]=", pack $ show x]
    , case weight_unit of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_variant[weight_unit]=", pack $ show x]
    ]

instance HasBarcode ProductVariant where
  theBarcodeMaybe = barcode                       
  setBarcode x y = y { barcode = Just x }

instance HasCompareAtPrice ProductVariant where
  theCompareAtPriceMaybe = compare_at_price              
  setCompareAtPrice x y = y { compare_at_price = Just x }

instance HasCreatedAt ProductVariant where
  theCreatedAtMaybe = created_at                    
  setCreatedAt x y = y { created_at = Just x }

instance HasFulfillmentServiceHandle ProductVariant where
  theFulfillmentServiceHandleMaybe = fulfillment_service           
  setFulfillmentServiceHandle x y = y { fulfillment_service = Just x }

instance HasGrams ProductVariant where
  theGramsMaybe = grams                         
  setGrams x y = y { grams = Just x }

instance HasID ProductVariant where
  theIDMaybe = id                            
  setID x y = y { id = Just x }

instance HasImageID ProductVariant where
  theImageIDMaybe = image_id                      
  setImageID x y = y { image_id = Just x }

instance HasInventoryManagement ProductVariant where
  theInventoryManagementMaybe = inventory_management          
  setInventoryManagement x y = y { inventory_management = Just x }

instance HasInventoryPolicy ProductVariant where
  theInventoryPolicyMaybe = inventory_policy              
  setInventoryPolicy x y = y { inventory_policy = Just x }

instance HasInventoryQuantity ProductVariant where
  theInventoryQuantityMaybe = inventory_quantity            
  setInventoryQuantity x y = y { inventory_quantity = Just x }

instance HasOldInventoryQuantity ProductVariant where
  theOldInventoryQuantityMaybe = old_inventory_quantity        
  setOldInventoryQuantity x y = y { old_inventory_quantity = Just x }

instance HasInventoryQuantityAdjustment ProductVariant where
  theInventoryQuantityAdjustmentMaybe = inventory_quantity_adjustment 
  setInventoryQuantityAdjustment x y = y { inventory_quantity_adjustment = Just x }

instance HasOption1 ProductVariant where
  theOption1Maybe = option1                       
  setOption1 x y = y { option1 = Just x }

instance HasOption2 ProductVariant where
  theOption2Maybe = option2                       
  setOption2 x y = y { option2 = Just x }

instance HasOption3 ProductVariant where
  theOption3Maybe = option3                       
  setOption3 x y = y { option3 = Just x }

instance HasPosition ProductVariant where
  thePositionMaybe = position                      
  setPosition x y = y { position = Just x }

instance HasPrice ProductVariant where
  thePriceMaybe = price                         
  setPrice x y = y { price = Just x }

instance HasProductID ProductVariant where
  theProductIDMaybe = product_id                    
  setProductID x y = y { product_id = Just x }

instance HasRequiresShipping ProductVariant where
  theRequiresShippingMaybe = requires_shipping             
  setRequiresShipping x y = y { requires_shipping = Just x }

instance HasSKU ProductVariant where
  theSKUMaybe = sku                           
  setSKU x y = y { sku = Just x }

instance HasTaxable ProductVariant where
  theTaxableMaybe = taxable                       
  setTaxable x y = y { taxable = Just x }

instance HasTitle ProductVariant where
  theTitleMaybe = title                         
  setTitle x y = y { title = Just x }

instance HasUpdatedAt ProductVariant where
  theUpdatedAtMaybe = updated_at                    
  setUpdatedAt x y = y { updated_at = Just x }

instance HasWeight ProductVariant where
  theWeightMaybe = weight                        
  setWeight x y = y { weight = Just x }

instance HasWeightUnit ProductVariant where
  theWeightUnitMaybe = weight_unit                   
  setWeightUnit x y = y { weight_unit = Just x }

