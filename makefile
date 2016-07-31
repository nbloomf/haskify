install: gen
	/app/halcyon/halcyon install /home/nathan/code/haskify

gen: FORCE
	gen/generateDataModule.sh ApplicationCharge "application_charge"
	gen/generateDataModule.sh Article article
	gen/generateDataModule.sh Asset asset
	gen/generateDataModule.sh Blog blog
	gen/generateDataModule.sh CarrierService "carrier_service"
	gen/generateDataModule.sh CarrierServiceItem "carrier_service_item"
	gen/generateDataModule.sh CarrierServiceLocation "carrier_service_location"
	gen/generateDataModule.sh CarrierServiceRate "carrier_service_rate"
	gen/generateDataModule.sh CarrierServiceRequest "carrier_service_request"
	gen/generateDataModule.sh Collect collect
	gen/generateDataModule.sh CustomCollection "custom_collection"
	gen/generateDataModule.sh Customer customer
	gen/generateDataModule.sh CustomerAddress "customer_address"
	gen/generateDataModule.sh Comment comment
	gen/generateDataModule.sh Country country
	gen/generateDataModule.sh FulfillmentService "fulfillment_service"
	gen/generateDataModule.sh Image image
	gen/generateDataModule.sh Location location
	gen/generateDataModule.sh Metafield metafield
	gen/generateDataModule.sh Page page
	gen/generateDataModule.sh Policy policy
	gen/generateDataModule.sh Product product
	gen/generateDataModule.sh ProductVariant "product_variant"
	gen/generateDataModule.sh ProductImage "product_image"
	gen/generateDataModule.sh Province province
	gen/generateDataModule.sh Redirect redirect
	gen/generateDataModule.sh Rule rule
	gen/generateDataModule.sh ScriptTag "script_tag"
	gen/generateDataModule.sh SmartCollection "smart_collection"
	gen/generateDataModule.sh Theme theme
	gen/generateDataModule.sh Transaction transaction
	gen/generateDataModule.sh UsageCharge "usage_charge"

	gen/generateDataImportModule.sh

	gen/generateValueModule.sh ApplicationChargeStatus
	gen/generateValueModule.sh ApplicationChargeTest
	gen/generateValueModule.sh CarrierServiceType
	gen/generateValueModule.sh Column
	gen/generateValueModule.sh Commentable
	gen/generateValueModule.sh CustomerState
	gen/generateValueModule.sh DisplayScope
	gen/generateValueModule.sh DOMEvent
	gen/generateValueModule.sh Format
	gen/generateValueModule.sh InventoryPolicy
	gen/generateValueModule.sh InventoryManagement
	gen/generateValueModule.sh PaymentDetails
	gen/generateValueModule.sh PublishedScope
	gen/generateValueModule.sh PublishedStatus
	gen/generateValueModule.sh Relation
	gen/generateValueModule.sh ShipmentStatus
	gen/generateValueModule.sh SortOrder
	gen/generateValueModule.sh ThemeRole
	gen/generateValueModule.sh TransactionErrorCode
	gen/generateValueModule.sh TransactionKind
	gen/generateValueModule.sh TransactionReceipt
	gen/generateValueModule.sh TransactionStatus
	gen/generateValueModule.sh ValueType

	gen/generateValueImportModule.sh

	gen/generateOptModule.sh GetBlogsOpt
	gen/generateOptModule.sh GetBlogsIDArticlesOpt
	gen/generateOptModule.sh GetBlogsIDArticlesCountOpt
	gen/generateOptModule.sh GetBlogsIDArticlesTagsOpt
	gen/generateOptModule.sh GetMetafieldsOpt
	gen/generateOptModule.sh GetProductsOpt
	gen/generateOptModule.sh GetProductsCountOpt
	gen/generateOptModule.sh GetProductsIDOpt
	gen/generateOptModule.sh GetProductsIDVariantsOpt

	gen/generateOptImportModule.sh

	gen/generateDataFieldsModule.sh
	gen/generateSingletonModule.sh

	cp -r temp/Data/ src/Haskify
	cp -r temp/Value/ src/Haskify
	cp -r temp/Opt/ src/Haskify
	cp temp/DataFields.hs src/Haskify
	cp temp/Singleton.hs src/Haskify
	cp temp/Data.hs src/Haskify
	cp temp/Value.hs src/Haskify
	cp temp/Opt.hs src/Haskify

	rm temp/DataImport.txt
	rm temp/ValueImport.txt
	rm temp/OptImport.txt

FORCE:
