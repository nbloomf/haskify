{-# LANGUAGE OverloadedStrings #-}

module Haskify.DataFields where

import Data.Text
import Haskify.Types
import Haskify.HaskifyM
import Data.Aeson (Object)

class HasAcceptsMarketing t where
  theAcceptsMarketingMaybe :: t -> Maybe Bool
  setAcceptsMarketing :: Bool -> t -> t

  theAcceptsMarketing :: t -> HaskifyM Bool
  theAcceptsMarketing x = case theAcceptsMarketingMaybe x of
    Nothing -> fieldDNE "AcceptsMarketing"
    Just y -> return y

class HasActive t where
  theActiveMaybe :: t -> Maybe Bool
  setActive :: Bool -> t -> t

  theActive :: t -> HaskifyM Bool
  theActive x = case theActiveMaybe x of
    Nothing -> fieldDNE "Active"
    Just y -> return y

class HasAddress1 t where
  theAddress1Maybe :: t -> Maybe Text
  setAddress1 :: Text -> t -> t

  theAddress1 :: t -> HaskifyM Text
  theAddress1 x = case theAddress1Maybe x of
    Nothing -> fieldDNE "Address1"
    Just y -> return y

class HasAddress2 t where
  theAddress2Maybe :: t -> Maybe Text
  setAddress2 :: Text -> t -> t

  theAddress2 :: t -> HaskifyM Text
  theAddress2 x = case theAddress2Maybe x of
    Nothing -> fieldDNE "Address2"
    Just y -> return y

class HasAddress3 t where
  theAddress3Maybe :: t -> Maybe Text
  setAddress3 :: Text -> t -> t

  theAddress3 :: t -> HaskifyM Text
  theAddress3 x = case theAddress3Maybe x of
    Nothing -> fieldDNE "Address3"
    Just y -> return y

class HasAddressType t where
  theAddressTypeMaybe :: t -> Maybe Text
  setAddressType :: Text -> t -> t

  theAddressType :: t -> HaskifyM Text
  theAddressType x = case theAddressTypeMaybe x of
    Nothing -> fieldDNE "AddressType"
    Just y -> return y

class HasAmount t where
  theAmountMaybe :: t -> Maybe Text
  setAmount :: Text -> t -> t

  theAmount :: t -> HaskifyM Text
  theAmount x = case theAmountMaybe x of
    Nothing -> fieldDNE "Amount"
    Just y -> return y

class HasAuthor t where
  theAuthorMaybe :: t -> Maybe Text
  setAuthor :: Text -> t -> t

  theAuthor :: t -> HaskifyM Text
  theAuthor x = case theAuthorMaybe x of
    Nothing -> fieldDNE "Author"
    Just y -> return y

class HasArticleID t where
  theArticleIDMaybe :: t -> Maybe IDNumber
  setArticleID :: IDNumber -> t -> t

  theArticleID :: t -> HaskifyM IDNumber
  theArticleID x = case theArticleIDMaybe x of
    Nothing -> fieldDNE "ArticleID"
    Just y -> return y

class HasAttachment t where
  theAttachmentMaybe :: t -> Maybe Text
  setAttachment :: Text -> t -> t

  theAttachment :: t -> HaskifyM Text
  theAttachment x = case theAttachmentMaybe x of
    Nothing -> fieldDNE "Attachment"
    Just y -> return y

class HasAuthorization t where
  theAuthorizationMaybe :: t -> Maybe Text
  setAuthorization :: Text -> t -> t

  theAuthorization :: t -> HaskifyM Text
  theAuthorization x = case theAuthorizationMaybe x of
    Nothing -> fieldDNE "Authorization"
    Just y -> return y

class HasBarcode t where
  theBarcodeMaybe :: t -> Maybe Text
  setBarcode :: Text -> t -> t

  theBarcode :: t -> HaskifyM Text
  theBarcode x = case theBarcodeMaybe x of
    Nothing -> fieldDNE "Barcode"
    Just y -> return y

class HasBlogID t where
  theBlogIDMaybe :: t -> Maybe IDNumber
  setBlogID :: IDNumber -> t -> t

  theBlogID :: t -> HaskifyM IDNumber
  theBlogID x = case theBlogIDMaybe x of
    Nothing -> fieldDNE "BlogID"
    Just y -> return y

class HasBody t where
  theBodyMaybe :: t -> Maybe Text
  setBody :: Text -> t -> t

  theBody :: t -> HaskifyM Text
  theBody x = case theBodyMaybe x of
    Nothing -> fieldDNE "Body"
    Just y -> return y

class HasBodyHTML t where
  theBodyHTMLMaybe :: t -> Maybe Text
  setBodyHTML :: Text -> t -> t

  theBodyHTML :: t -> HaskifyM Text
  theBodyHTML x = case theBodyHTMLMaybe x of
    Nothing -> fieldDNE "BodyHTML"
    Just y -> return y

class HasCallbackURL t where
  theCallbackURLMaybe :: t -> Maybe Text
  setCallbackURL :: Text -> t -> t

  theCallbackURL :: t -> HaskifyM Text
  theCallbackURL x = case theCallbackURLMaybe x of
    Nothing -> fieldDNE "CallbackURL"
    Just y -> return y

class HasCity t where
  theCityMaybe :: t -> Maybe Text
  setCity :: Text -> t -> t

  theCity :: t -> HaskifyM Text
  theCity x = case theCityMaybe x of
    Nothing -> fieldDNE "City"
    Just y -> return y

class HasCompany t where
  theCompanyMaybe :: t -> Maybe Text
  setCompany :: Text -> t -> t

  theCompany :: t -> HaskifyM Text
  theCompany x = case theCompanyMaybe x of
    Nothing -> fieldDNE "Company"
    Just y -> return y

class HasConfirmationURL t where
  theConfirmationURLMaybe :: t -> Maybe Text
  setConfirmationURL :: Text -> t -> t

  theConfirmationURL :: t -> HaskifyM Text
  theConfirmationURL x = case theConfirmationURLMaybe x of
    Nothing -> fieldDNE "ConfirmationURL"
    Just y -> return y

class HasContentType t where
  theContentTypeMaybe :: t -> Maybe Text
  setContentType :: Text -> t -> t

  theContentType :: t -> HaskifyM Text
  theContentType x = case theContentTypeMaybe x of
    Nothing -> fieldDNE "ContentType"
    Just y -> return y

class HasCode t where
  theCodeMaybe :: t -> Maybe Text
  setCode :: Text -> t -> t

  theCode :: t -> HaskifyM Text
  theCode x = case theCodeMaybe x of
    Nothing -> fieldDNE "Code"
    Just y -> return y

class HasCompanyName t where
  theCompanyNameMaybe :: t -> Maybe Text
  setCompanyName :: Text -> t -> t

  theCompanyName :: t -> HaskifyM Text
  theCompanyName x = case theCompanyNameMaybe x of
    Nothing -> fieldDNE "CompanyName"
    Just y -> return y

class HasCompareAtPrice t where
  theCompareAtPriceMaybe :: t -> Maybe Text
  setCompareAtPrice :: Text -> t -> t

  theCompareAtPrice :: t -> HaskifyM Text
  theCompareAtPrice x = case theCompareAtPriceMaybe x of
    Nothing -> fieldDNE "CompareAtPrice"
    Just y -> return y

class HasCondition t where
  theConditionMaybe :: t -> Maybe Text
  setCondition :: Text -> t -> t

  theCondition :: t -> HaskifyM Text
  theCondition x = case theConditionMaybe x of
    Nothing -> fieldDNE "Condition"
    Just y -> return y

class HasCollectionID t where
  theCollectionIDMaybe :: t -> Maybe IDNumber
  setCollectionID :: IDNumber -> t -> t

  theCollectionID :: t -> HaskifyM IDNumber
  theCollectionID x = case theCollectionIDMaybe x of
    Nothing -> fieldDNE "CollectionID"
    Just y -> return y

class HasCountryText t where
  theCountryTextMaybe :: t -> Maybe Text
  setCountryText :: Text -> t -> t

  theCountryText :: t -> HaskifyM Text
  theCountryText x = case theCountryTextMaybe x of
    Nothing -> fieldDNE "CountryText"
    Just y -> return y

class HasCountryID t where
  theCountryIDMaybe :: t -> Maybe IDNumber
  setCountryID :: IDNumber -> t -> t

  theCountryID :: t -> HaskifyM IDNumber
  theCountryID x = case theCountryIDMaybe x of
    Nothing -> fieldDNE "CountryID"
    Just y -> return y

class HasCountryCode t where
  theCountryCodeMaybe :: t -> Maybe Text
  setCountryCode :: Text -> t -> t

  theCountryCode :: t -> HaskifyM Text
  theCountryCode x = case theCountryCodeMaybe x of
    Nothing -> fieldDNE "CountryCode"
    Just y -> return y

class HasCountryName t where
  theCountryNameMaybe :: t -> Maybe Text
  setCountryName :: Text -> t -> t

  theCountryName :: t -> HaskifyM Text
  theCountryName x = case theCountryNameMaybe x of
    Nothing -> fieldDNE "CountryName"
    Just y -> return y

class HasCreatedAt t where
  theCreatedAtMaybe :: t -> Maybe DateTime
  setCreatedAt :: DateTime -> t -> t

  theCreatedAt :: t -> HaskifyM DateTime
  theCreatedAt x = case theCreatedAtMaybe x of
    Nothing -> fieldDNE "CreatedAt"
    Just y -> return y

class HasCurrency t where
  theCurrencyMaybe :: t -> Maybe Text
  setCurrency :: Text -> t -> t

  theCurrency :: t -> HaskifyM Text
  theCurrency x = case theCurrencyMaybe x of
    Nothing -> fieldDNE "Currency"
    Just y -> return y

class HasDescription t where
  theDescriptionMaybe :: t -> Maybe Text
  setDescription :: Text -> t -> t

  theDescription :: t -> HaskifyM Text
  theDescription x = case theDescriptionMaybe x of
    Nothing -> fieldDNE "Description"
    Just y -> return y

class HasDisjunctive t where
  theDisjunctiveMaybe :: t -> Maybe Bool
  setDisjunctive :: Bool -> t -> t

  theDisjunctive :: t -> HaskifyM Bool
  theDisjunctive x = case theDisjunctiveMaybe x of
    Nothing -> fieldDNE "Disjunctive"
    Just y -> return y

class HasDeviceID t where
  theDeviceIDMaybe :: t -> Maybe IDNumber
  setDeviceID :: IDNumber -> t -> t

  theDeviceID :: t -> HaskifyM IDNumber
  theDeviceID x = case theDeviceIDMaybe x of
    Nothing -> fieldDNE "DeviceID"
    Just y -> return y

class HasEmail t where
  theEmailMaybe :: t -> Maybe Text
  setEmail :: Text -> t -> t

  theEmail :: t -> HaskifyM Text
  theEmail x = case theEmailMaybe x of
    Nothing -> fieldDNE "Email"
    Just y -> return y

class HasFax t where
  theFaxMaybe :: t -> Maybe Text
  setFax :: Text -> t -> t

  theFax :: t -> HaskifyM Text
  theFax x = case theFaxMaybe x of
    Nothing -> fieldDNE "Fax"
    Just y -> return y

class HasFeatured t where
  theFeaturedMaybe :: t -> Maybe Bool
  setFeatured :: Bool -> t -> t

  theFeatured :: t -> HaskifyM Bool
  theFeatured x = case theFeaturedMaybe x of
    Nothing -> fieldDNE "Featured"
    Just y -> return y

class HasFeedburner t where
  theFeedburnerMaybe :: t -> Maybe Text
  setFeedburner :: Text -> t -> t

  theFeedburner :: t -> HaskifyM Text
  theFeedburner x = case theFeedburnerMaybe x of
    Nothing -> fieldDNE "Feedburner"
    Just y -> return y

class HasFeedburnerLocation t where
  theFeedburnerLocationMaybe :: t -> Maybe Text
  setFeedburnerLocation :: Text -> t -> t

  theFeedburnerLocation :: t -> HaskifyM Text
  theFeedburnerLocation x = case theFeedburnerLocationMaybe x of
    Nothing -> fieldDNE "FeedburnerLocation"
    Just y -> return y

class HasFirstName t where
  theFirstNameMaybe :: t -> Maybe Text
  setFirstName :: Text -> t -> t

  theFirstName :: t -> HaskifyM Text
  theFirstName x = case theFirstNameMaybe x of
    Nothing -> fieldDNE "FirstName"
    Just y -> return y

class HasFulfillmentServiceHandle t where
  theFulfillmentServiceHandleMaybe :: t -> Maybe Text
  setFulfillmentServiceHandle :: Text -> t -> t

  theFulfillmentServiceHandle :: t -> HaskifyM Text
  theFulfillmentServiceHandle x = case theFulfillmentServiceHandleMaybe x of
    Nothing -> fieldDNE "FulfillmentServiceHandle"
    Just y -> return y

class HasGateway t where
  theGatewayMaybe :: t -> Maybe Text
  setGateway :: Text -> t -> t

  theGateway :: t -> HaskifyM Text
  theGateway x = case theGatewayMaybe x of
    Nothing -> fieldDNE "Gateway"
    Just y -> return y

class HasGrams t where
  theGramsMaybe :: t -> Maybe Int
  setGrams :: Int -> t -> t

  theGrams :: t -> HaskifyM Int
  theGrams x = case theGramsMaybe x of
    Nothing -> fieldDNE "Grams"
    Just y -> return y

class HasHandle t where
  theHandleMaybe :: t -> Maybe Text
  setHandle :: Text -> t -> t

  theHandle :: t -> HaskifyM Text
  theHandle x = case theHandleMaybe x of
    Nothing -> fieldDNE "Handle"
    Just y -> return y

class HasID t where
  theIDMaybe :: t -> Maybe IDNumber
  setID :: IDNumber -> t -> t

  theID :: t -> HaskifyM IDNumber
  theID x = case theIDMaybe x of
    Nothing -> fieldDNE "ID"
    Just y -> return y

class HasIP t where
  theIPMaybe :: t -> Maybe Text
  setIP :: Text -> t -> t

  theIP :: t -> HaskifyM Text
  theIP x = case theIPMaybe x of
    Nothing -> fieldDNE "IP"
    Just y -> return y

class HasImageID t where
  theImageIDMaybe :: t -> Maybe IDNumber
  setImageID :: IDNumber -> t -> t

  theImageID :: t -> HaskifyM IDNumber
  theImageID x = case theImageIDMaybe x of
    Nothing -> fieldDNE "ImageID"
    Just y -> return y

class HasInventoryQuantity t where
  theInventoryQuantityMaybe :: t -> Maybe Int
  setInventoryQuantity :: Int -> t -> t

  theInventoryQuantity :: t -> HaskifyM Int
  theInventoryQuantity x = case theInventoryQuantityMaybe x of
    Nothing -> fieldDNE "InventoryQuantity"
    Just y -> return y

class HasInventoryQuantityAdjustment t where
  theInventoryQuantityAdjustmentMaybe :: t -> Maybe Int
  setInventoryQuantityAdjustment :: Int -> t -> t

  theInventoryQuantityAdjustment :: t -> HaskifyM Int
  theInventoryQuantityAdjustment x = case theInventoryQuantityAdjustmentMaybe x of
    Nothing -> fieldDNE "InventoryQuantityAdjustment"
    Just y -> return y

class HasKey t where
  theKeyMaybe :: t -> Maybe Text
  setKey :: Text -> t -> t

  theKey :: t -> HaskifyM Text
  theKey x = case theKeyMaybe x of
    Nothing -> fieldDNE "Key"
    Just y -> return y

class HasLastName t where
  theLastNameMaybe :: t -> Maybe Text
  setLastName :: Text -> t -> t

  theLastName :: t -> HaskifyM Text
  theLastName x = case theLastNameMaybe x of
    Nothing -> fieldDNE "LastName"
    Just y -> return y

class HasLastOrderID t where
  theLastOrderIDMaybe :: t -> Maybe IDNumber
  setLastOrderID :: IDNumber -> t -> t

  theLastOrderID :: t -> HaskifyM IDNumber
  theLastOrderID x = case theLastOrderIDMaybe x of
    Nothing -> fieldDNE "LastOrderID"
    Just y -> return y

class HasLastOrderName t where
  theLastOrderNameMaybe :: t -> Maybe Text
  setLastOrderName :: Text -> t -> t

  theLastOrderName :: t -> HaskifyM Text
  theLastOrderName x = case theLastOrderNameMaybe x of
    Nothing -> fieldDNE "LastOrderName"
    Just y -> return y

class HasMaxDeliveryDate t where
  theMaxDeliveryDateMaybe :: t -> Maybe DateTime
  setMaxDeliveryDate :: DateTime -> t -> t

  theMaxDeliveryDate :: t -> HaskifyM DateTime
  theMaxDeliveryDate x = case theMaxDeliveryDateMaybe x of
    Nothing -> fieldDNE "MaxDeliveryDate"
    Just y -> return y

class HasMinDeliveryDate t where
  theMinDeliveryDateMaybe :: t -> Maybe DateTime
  setMinDeliveryDate :: DateTime -> t -> t

  theMinDeliveryDate :: t -> HaskifyM DateTime
  theMinDeliveryDate x = case theMinDeliveryDateMaybe x of
    Nothing -> fieldDNE "MinDeliveryDate"
    Just y -> return y

class HasMetafieldsGlobalTitle t where
  theMetafieldsGlobalTitleMaybe :: t -> Maybe Text
  setMetafieldsGlobalTitle :: Text -> t -> t

  theMetafieldsGlobalTitle :: t -> HaskifyM Text
  theMetafieldsGlobalTitle x = case theMetafieldsGlobalTitleMaybe x of
    Nothing -> fieldDNE "MetafieldsGlobalTitle"
    Just y -> return y

class HasMetafieldsGlobalDescription t where
  theMetafieldsGlobalDescriptionMaybe :: t -> Maybe Text
  setMetafieldsGlobalDescription :: Text -> t -> t

  theMetafieldsGlobalDescription :: t -> HaskifyM Text
  theMetafieldsGlobalDescription x = case theMetafieldsGlobalDescriptionMaybe x of
    Nothing -> fieldDNE "MetafieldsGlobalDescription"
    Just y -> return y

class HasMultipassIdentifier t where
  theMultipassIdentifierMaybe :: t -> Maybe Text
  setMultipassIdentifier :: Text -> t -> t

  theMultipassIdentifier :: t -> HaskifyM Text
  theMultipassIdentifier x = case theMultipassIdentifierMaybe x of
    Nothing -> fieldDNE "MultipassIdentifier"
    Just y -> return y

class HasName t where
  theNameMaybe :: t -> Maybe Text
  setName :: Text -> t -> t

  theName :: t -> HaskifyM Text
  theName x = case theNameMaybe x of
    Nothing -> fieldDNE "Name"
    Just y -> return y

class HasNamespace t where
  theNamespaceMaybe :: t -> Maybe Text
  setNamespace :: Text -> t -> t

  theNamespace :: t -> HaskifyM Text
  theNamespace x = case theNamespaceMaybe x of
    Nothing -> fieldDNE "Namespace"
    Just y -> return y

class HasNote t where
  theNoteMaybe :: t -> Maybe Text
  setNote :: Text -> t -> t

  theNote :: t -> HaskifyM Text
  theNote x = case theNoteMaybe x of
    Nothing -> fieldDNE "Note"
    Just y -> return y

class HasOldInventoryQuantity t where
  theOldInventoryQuantityMaybe :: t -> Maybe Int
  setOldInventoryQuantity :: Int -> t -> t

  theOldInventoryQuantity :: t -> HaskifyM Int
  theOldInventoryQuantity x = case theOldInventoryQuantityMaybe x of
    Nothing -> fieldDNE "OldInventoryQuantity"
    Just y -> return y

class HasOption1 t where
  theOption1Maybe :: t -> Maybe Text
  setOption1 :: Text -> t -> t

  theOption1 :: t -> HaskifyM Text
  theOption1 x = case theOption1Maybe x of
    Nothing -> fieldDNE "Option1"
    Just y -> return y

class HasOption2 t where
  theOption2Maybe :: t -> Maybe Text
  setOption2 :: Text -> t -> t

  theOption2 :: t -> HaskifyM Text
  theOption2 x = case theOption2Maybe x of
    Nothing -> fieldDNE "Option2"
    Just y -> return y

class HasOption3 t where
  theOption3Maybe :: t -> Maybe Text
  setOption3 :: Text -> t -> t

  theOption3 :: t -> HaskifyM Text
  theOption3 x = case theOption3Maybe x of
    Nothing -> fieldDNE "Option3"
    Just y -> return y

class HasOptions t where
  theOptionsMaybe :: t -> Maybe [Object]
  setOptions :: [Object] -> t -> t

  theOptions :: t -> HaskifyM [Object]
  theOptions x = case theOptionsMaybe x of
    Nothing -> fieldDNE "Options"
    Just y -> return y

class HasOrdersCount t where
  theOrdersCountMaybe :: t -> Maybe Int
  setOrdersCount :: Int -> t -> t

  theOrdersCount :: t -> HaskifyM Int
  theOrdersCount x = case theOrdersCountMaybe x of
    Nothing -> fieldDNE "OrdersCount"
    Just y -> return y

class HasOrderID t where
  theOrderIDMaybe :: t -> Maybe IDNumber
  setOrderID :: IDNumber -> t -> t

  theOrderID :: t -> HaskifyM IDNumber
  theOrderID x = case theOrderIDMaybe x of
    Nothing -> fieldDNE "OrderID"
    Just y -> return y

class HasOwnerID t where
  theOwnerIDMaybe :: t -> Maybe IDNumber
  setOwnerID :: IDNumber -> t -> t

  theOwnerID :: t -> HaskifyM IDNumber
  theOwnerID x = case theOwnerIDMaybe x of
    Nothing -> fieldDNE "OwnerID"
    Just y -> return y

class HasOwnerResource t where
  theOwnerResourceMaybe :: t -> Maybe Text
  setOwnerResource :: Text -> t -> t

  theOwnerResource :: t -> HaskifyM Text
  theOwnerResource x = case theOwnerResourceMaybe x of
    Nothing -> fieldDNE "OwnerResource"
    Just y -> return y

class HasPath t where
  thePathMaybe :: t -> Maybe Text
  setPath :: Text -> t -> t

  thePath :: t -> HaskifyM Text
  thePath x = case thePathMaybe x of
    Nothing -> fieldDNE "Path"
    Just y -> return y

class HasPhone t where
  thePhoneMaybe :: t -> Maybe Text
  setPhone :: Text -> t -> t

  thePhone :: t -> HaskifyM Text
  thePhone x = case thePhoneMaybe x of
    Nothing -> fieldDNE "Phone"
    Just y -> return y

class HasPosition t where
  thePositionMaybe :: t -> Maybe Int
  setPosition :: Int -> t -> t

  thePosition :: t -> HaskifyM Int
  thePosition x = case thePositionMaybe x of
    Nothing -> fieldDNE "Position"
    Just y -> return y

class HasPostalCode t where
  thePostalCodeMaybe :: t -> Maybe Text
  setPostalCode :: Text -> t -> t

  thePostalCode :: t -> HaskifyM Text
  thePostalCode x = case thePostalCodeMaybe x of
    Nothing -> fieldDNE "PostalCode"
    Just y -> return y

class HasPreviewable t where
  thePreviewableMaybe :: t -> Maybe Bool
  setPreviewable :: Bool -> t -> t

  thePreviewable :: t -> HaskifyM Bool
  thePreviewable x = case thePreviewableMaybe x of
    Nothing -> fieldDNE "Previewable"
    Just y -> return y

class HasProcessing t where
  theProcessingMaybe :: t -> Maybe Bool
  setProcessing :: Bool -> t -> t

  theProcessing :: t -> HaskifyM Bool
  theProcessing x = case theProcessingMaybe x of
    Nothing -> fieldDNE "Processing"
    Just y -> return y

class HasPrice t where
  thePriceMaybe :: t -> Maybe Text
  setPrice :: Text -> t -> t

  thePrice :: t -> HaskifyM Text
  thePrice x = case thePriceMaybe x of
    Nothing -> fieldDNE "Price"
    Just y -> return y

class HasPriceInt t where
  thePriceIntMaybe :: t -> Maybe Int
  setPriceInt :: Int -> t -> t

  thePriceInt :: t -> HaskifyM Int
  thePriceInt x = case thePriceIntMaybe x of
    Nothing -> fieldDNE "PriceInt"
    Just y -> return y

class HasProductID t where
  theProductIDMaybe :: t -> Maybe IDNumber
  setProductID :: IDNumber -> t -> t

  theProductID :: t -> HaskifyM IDNumber
  theProductID x = case theProductIDMaybe x of
    Nothing -> fieldDNE "ProductID"
    Just y -> return y

class HasProductType t where
  theProductTypeMaybe :: t -> Maybe Text
  setProductType :: Text -> t -> t

  theProductType :: t -> HaskifyM Text
  theProductType x = case theProductTypeMaybe x of
    Nothing -> fieldDNE "ProductType"
    Just y -> return y

class HasProviderID t where
  theProviderIDMaybe :: t -> Maybe IDNumber
  setProviderID :: IDNumber -> t -> t

  theProviderID :: t -> HaskifyM IDNumber
  theProviderID x = case theProviderIDMaybe x of
    Nothing -> fieldDNE "ProviderID"
    Just y -> return y

class HasProvinceName t where
  theProvinceNameMaybe :: t -> Maybe Text
  setProvinceName :: Text -> t -> t

  theProvinceName :: t -> HaskifyM Text
  theProvinceName x = case theProvinceNameMaybe x of
    Nothing -> fieldDNE "ProvinceName"
    Just y -> return y

class HasProvinceCode t where
  theProvinceCodeMaybe :: t -> Maybe Text
  setProvinceCode :: Text -> t -> t

  theProvinceCode :: t -> HaskifyM Text
  theProvinceCode x = case theProvinceCodeMaybe x of
    Nothing -> fieldDNE "ProvinceCode"
    Just y -> return y

class HasPublicURL t where
  thePublicURLMaybe :: t -> Maybe Text
  setPublicURL :: Text -> t -> t

  thePublicURL :: t -> HaskifyM Text
  thePublicURL x = case thePublicURLMaybe x of
    Nothing -> fieldDNE "PublicURL"
    Just y -> return y

class HasPublished t where
  thePublishedMaybe :: t -> Maybe Bool
  setPublished :: Bool -> t -> t

  thePublished :: t -> HaskifyM Bool
  thePublished x = case thePublishedMaybe x of
    Nothing -> fieldDNE "Published"
    Just y -> return y

class HasPublishedAt t where
  thePublishedAtMaybe :: t -> Maybe DateTime
  setPublishedAt :: DateTime -> t -> t

  thePublishedAt :: t -> HaskifyM DateTime
  thePublishedAt x = case thePublishedAtMaybe x of
    Nothing -> fieldDNE "PublishedAt"
    Just y -> return y

class HasQuantity t where
  theQuantityMaybe :: t -> Maybe Int
  setQuantity :: Int -> t -> t

  theQuantity :: t -> HaskifyM Int
  theQuantity x = case theQuantityMaybe x of
    Nothing -> fieldDNE "Quantity"
    Just y -> return y

class HasRecurringApplicationChargeID t where
  theRecurringApplicationChargeIDMaybe :: t -> Maybe IDNumber
  setRecurringApplicationChargeID :: IDNumber -> t -> t

  theRecurringApplicationChargeID :: t -> HaskifyM IDNumber
  theRecurringApplicationChargeID x = case theRecurringApplicationChargeIDMaybe x of
    Nothing -> fieldDNE "RecurringApplicationChargeID"
    Just y -> return y

class HasRequiresShipping t where
  theRequiresShippingMaybe :: t -> Maybe Bool
  setRequiresShipping :: Bool -> t -> t

  theRequiresShipping :: t -> HaskifyM Bool
  theRequiresShipping x = case theRequiresShippingMaybe x of
    Nothing -> fieldDNE "RequiresShipping"
    Just y -> return y

class HasRequiresShippingMethod t where
  theRequiresShippingMethodMaybe :: t -> Maybe Bool
  setRequiresShippingMethod :: Bool -> t -> t

  theRequiresShippingMethod :: t -> HaskifyM Bool
  theRequiresShippingMethod x = case theRequiresShippingMethodMaybe x of
    Nothing -> fieldDNE "RequiresShippingMethod"
    Just y -> return y

class HasReturnURL t where
  theReturnURLMaybe :: t -> Maybe Text
  setReturnURL :: Text -> t -> t

  theReturnURL :: t -> HaskifyM Text
  theReturnURL x = case theReturnURLMaybe x of
    Nothing -> fieldDNE "ReturnURL"
    Just y -> return y

class HasServiceCode t where
  theServiceCodeMaybe :: t -> Maybe Text
  setServiceCode :: Text -> t -> t

  theServiceCode :: t -> HaskifyM Text
  theServiceCode x = case theServiceCodeMaybe x of
    Nothing -> fieldDNE "ServiceCode"
    Just y -> return y

class HasServiceName t where
  theServiceNameMaybe :: t -> Maybe Text
  setServiceName :: Text -> t -> t

  theServiceName :: t -> HaskifyM Text
  theServiceName x = case theServiceNameMaybe x of
    Nothing -> fieldDNE "ServiceName"
    Just y -> return y

class HasServiceDiscovery t where
  theServiceDiscoveryMaybe :: t -> Maybe Bool
  setServiceDiscovery :: Bool -> t -> t

  theServiceDiscovery :: t -> HaskifyM Bool
  theServiceDiscovery x = case theServiceDiscoveryMaybe x of
    Nothing -> fieldDNE "ServiceDiscovery"
    Just y -> return y

class HasSourceName t where
  theSourceNameMaybe :: t -> Maybe Text
  setSourceName :: Text -> t -> t

  theSourceName :: t -> HaskifyM Text
  theSourceName x = case theSourceNameMaybe x of
    Nothing -> fieldDNE "SourceName"
    Just y -> return y

class HasShippingZoneID t where
  theShippingZoneIDMaybe :: t -> Maybe IDNumber
  setShippingZoneID :: IDNumber -> t -> t

  theShippingZoneID :: t -> HaskifyM IDNumber
  theShippingZoneID x = case theShippingZoneIDMaybe x of
    Nothing -> fieldDNE "ShippingZoneID"
    Just y -> return y

class HasShopID t where
  theShopIDMaybe :: t -> Maybe IDNumber
  setShopID :: IDNumber -> t -> t

  theShopID :: t -> HaskifyM IDNumber
  theShopID x = case theShopIDMaybe x of
    Nothing -> fieldDNE "ShopID"
    Just y -> return y

class HasSize t where
  theSizeMaybe :: t -> Maybe Int
  setSize :: Int -> t -> t

  theSize :: t -> HaskifyM Int
  theSize x = case theSizeMaybe x of
    Nothing -> fieldDNE "Size"
    Just y -> return y

class HasSKU t where
  theSKUMaybe :: t -> Maybe Text
  setSKU :: Text -> t -> t

  theSKU :: t -> HaskifyM Text
  theSKU x = case theSKUMaybe x of
    Nothing -> fieldDNE "SKU"
    Just y -> return y

class HasSortValue t where
  theSortValueMaybe :: t -> Maybe Text
  setSortValue :: Text -> t -> t

  theSortValue :: t -> HaskifyM Text
  theSortValue x = case theSortValueMaybe x of
    Nothing -> fieldDNE "SortValue"
    Just y -> return y

class HasSourceKey t where
  theSourceKeyMaybe :: t -> Maybe Text
  setSourceKey :: Text -> t -> t

  theSourceKey :: t -> HaskifyM Text
  theSourceKey x = case theSourceKeyMaybe x of
    Nothing -> fieldDNE "SourceKey"
    Just y -> return y

class HasSrc t where
  theSrcMaybe :: t -> Maybe Text
  setSrc :: Text -> t -> t

  theSrc :: t -> HaskifyM Text
  theSrc x = case theSrcMaybe x of
    Nothing -> fieldDNE "Src"
    Just y -> return y

class HasStatus t where
  theStatusMaybe :: t -> Maybe Text
  setStatus :: Text -> t -> t

  theStatus :: t -> HaskifyM Text
  theStatus x = case theStatusMaybe x of
    Nothing -> fieldDNE "Status"
    Just y -> return y

class HasSummaryHTML t where
  theSummaryHTMLMaybe :: t -> Maybe Text
  setSummaryHTML :: Text -> t -> t

  theSummaryHTML :: t -> HaskifyM Text
  theSummaryHTML x = case theSummaryHTMLMaybe x of
    Nothing -> fieldDNE "SummaryHTML"
    Just y -> return y

class HasTags t where
  theTagsMaybe :: t -> Maybe Text
  setTags :: Text -> t -> t

  theTags :: t -> HaskifyM Text
  theTags x = case theTagsMaybe x of
    Nothing -> fieldDNE "Tags"
    Just y -> return y

class HasTax t where
  theTaxMaybe :: t -> Maybe Float
  setTax :: Float -> t -> t

  theTax :: t -> HaskifyM Float
  theTax x = case theTaxMaybe x of
    Nothing -> fieldDNE "Tax"
    Just y -> return y

class HasTaxable t where
  theTaxableMaybe :: t -> Maybe Bool
  setTaxable :: Bool -> t -> t

  theTaxable :: t -> HaskifyM Bool
  theTaxable x = case theTaxableMaybe x of
    Nothing -> fieldDNE "Taxable"
    Just y -> return y

class HasTaxExempt t where
  theTaxExemptMaybe :: t -> Maybe Bool
  setTaxExempt :: Bool -> t -> t

  theTaxExempt :: t -> HaskifyM Bool
  theTaxExempt x = case theTaxExemptMaybe x of
    Nothing -> fieldDNE "TaxExempt"
    Just y -> return y

class HasTaxName t where
  theTaxNameMaybe :: t -> Maybe Text
  setTaxName :: Text -> t -> t

  theTaxName :: t -> HaskifyM Text
  theTaxName x = case theTaxNameMaybe x of
    Nothing -> fieldDNE "TaxName"
    Just y -> return y

class HasTaxType t where
  theTaxTypeMaybe :: t -> Maybe Text
  setTaxType :: Text -> t -> t

  theTaxType :: t -> HaskifyM Text
  theTaxType x = case theTaxTypeMaybe x of
    Nothing -> fieldDNE "TaxType"
    Just y -> return y

class HasTaxPercentage t where
  theTaxPercentageMaybe :: t -> Maybe Float
  setTaxPercentage :: Float -> t -> t

  theTaxPercentage :: t -> HaskifyM Float
  theTaxPercentage x = case theTaxPercentageMaybe x of
    Nothing -> fieldDNE "TaxPercentage"
    Just y -> return y

class HasTarget t where
  theTargetMaybe :: t -> Maybe Text
  setTarget :: Text -> t -> t

  theTarget :: t -> HaskifyM Text
  theTarget x = case theTargetMaybe x of
    Nothing -> fieldDNE "Target"
    Just y -> return y

class HasTemplateSuffix t where
  theTemplateSuffixMaybe :: t -> Maybe Text
  setTemplateSuffix :: Text -> t -> t

  theTemplateSuffix :: t -> HaskifyM Text
  theTemplateSuffix x = case theTemplateSuffixMaybe x of
    Nothing -> fieldDNE "TemplateSuffix"
    Just y -> return y

class HasTest t where
  theTestMaybe :: t -> Maybe Bool
  setTest :: Bool -> t -> t

  theTest :: t -> HaskifyM Bool
  theTest x = case theTestMaybe x of
    Nothing -> fieldDNE "Test"
    Just y -> return y

class HasTitle t where
  theTitleMaybe :: t -> Maybe Text
  setTitle :: Text -> t -> t

  theTitle :: t -> HaskifyM Text
  theTitle x = case theTitleMaybe x of
    Nothing -> fieldDNE "Title"
    Just y -> return y

class HasThemeID t where
  theThemeIDMaybe :: t -> Maybe IDNumber
  setThemeID :: IDNumber -> t -> t

  theThemeID :: t -> HaskifyM IDNumber
  theThemeID x = case theThemeIDMaybe x of
    Nothing -> fieldDNE "ThemeID"
    Just y -> return y

class HasTotalSpent t where
  theTotalSpentMaybe :: t -> Maybe Text
  setTotalSpent :: Text -> t -> t

  theTotalSpent :: t -> HaskifyM Text
  theTotalSpent x = case theTotalSpentMaybe x of
    Nothing -> fieldDNE "TotalSpent"
    Just y -> return y

class HasTotalPrice t where
  theTotalPriceMaybe :: t -> Maybe Text
  setTotalPrice :: Text -> t -> t

  theTotalPrice :: t -> HaskifyM Text
  theTotalPrice x = case theTotalPriceMaybe x of
    Nothing -> fieldDNE "TotalPrice"
    Just y -> return y

class HasTrackingSupport t where
  theTrackingSupportMaybe :: t -> Maybe Bool
  setTrackingSupport :: Bool -> t -> t

  theTrackingSupport :: t -> HaskifyM Bool
  theTrackingSupport x = case theTrackingSupportMaybe x of
    Nothing -> fieldDNE "TrackingSupport"
    Just y -> return y

class HasUpdatedAt t where
  theUpdatedAtMaybe :: t -> Maybe DateTime
  setUpdatedAt :: DateTime -> t -> t

  theUpdatedAt :: t -> HaskifyM DateTime
  theUpdatedAt x = case theUpdatedAtMaybe x of
    Nothing -> fieldDNE "UpdatedAt"
    Just y -> return y

class HasURL t where
  theURLMaybe :: t -> Maybe Text
  setURL :: Text -> t -> t

  theURL :: t -> HaskifyM Text
  theURL x = case theURLMaybe x of
    Nothing -> fieldDNE "URL"
    Just y -> return y

class HasUserAgent t where
  theUserAgentMaybe :: t -> Maybe Text
  setUserAgent :: Text -> t -> t

  theUserAgent :: t -> HaskifyM Text
  theUserAgent x = case theUserAgentMaybe x of
    Nothing -> fieldDNE "UserAgent"
    Just y -> return y

class HasUserID t where
  theUserIDMaybe :: t -> Maybe IDNumber
  setUserID :: IDNumber -> t -> t

  theUserID :: t -> HaskifyM IDNumber
  theUserID x = case theUserIDMaybe x of
    Nothing -> fieldDNE "UserID"
    Just y -> return y

class HasValue t where
  theValueMaybe :: t -> Maybe Text
  setValue :: Text -> t -> t

  theValue :: t -> HaskifyM Text
  theValue x = case theValueMaybe x of
    Nothing -> fieldDNE "Value"
    Just y -> return y

class HasVariantIDs t where
  theVariantIDsMaybe :: t -> Maybe [IDNumber]
  setVariantIDs :: [IDNumber] -> t -> t

  theVariantIDs :: t -> HaskifyM [IDNumber]
  theVariantIDs x = case theVariantIDsMaybe x of
    Nothing -> fieldDNE "VariantIDs"
    Just y -> return y

class HasVendor t where
  theVendorMaybe :: t -> Maybe Text
  setVendor :: Text -> t -> t

  theVendor :: t -> HaskifyM Text
  theVendor x = case theVendorMaybe x of
    Nothing -> fieldDNE "Vendor"
    Just y -> return y

class HasVerifiedEmail t where
  theVerifiedEmailMaybe :: t -> Maybe Bool
  setVerifiedEmail :: Bool -> t -> t

  theVerifiedEmail :: t -> HaskifyM Bool
  theVerifiedEmail x = case theVerifiedEmailMaybe x of
    Nothing -> fieldDNE "VerifiedEmail"
    Just y -> return y

class HasWeight t where
  theWeightMaybe :: t -> Maybe Float
  setWeight :: Float -> t -> t

  theWeight :: t -> HaskifyM Float
  theWeight x = case theWeightMaybe x of
    Nothing -> fieldDNE "Weight"
    Just y -> return y

class HasWeightUnit t where
  theWeightUnitMaybe :: t -> Maybe Text
  setWeightUnit :: Text -> t -> t

  theWeightUnit :: t -> HaskifyM Text
  theWeightUnit x = case theWeightUnitMaybe x of
    Nothing -> fieldDNE "WeightUnit"
    Just y -> return y

class HasZip t where
  theZipMaybe :: t -> Maybe Text
  setZip :: Text -> t -> t

  theZip :: t -> HaskifyM Text
  theZip x = case theZipMaybe x of
    Nothing -> fieldDNE "Zip"
    Just y -> return y

