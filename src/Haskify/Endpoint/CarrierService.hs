{-# LANGUAGE OverloadedStrings #-}

module Haskify.Endpoint.CarrierService where

import Haskify.HaskifyM
import Haskify.Data
import Haskify.Singleton
import Haskify.Opt
import Haskify.Types



{- #GetCarrierServices -}

getCarrierServices
  :: HaskifyM [CarrierService]
getCarrierServices = do
  let path = "/admin/carrier_services.json"
  let args = ""
  urlGet path args >>= decodeCarrierServicesSingleton



{- #PostCarrierServices -}

postCarrierServices
  :: (CarrierService -> CarrierService) -> HaskifyM CarrierService
postCarrierServices f = do
  let path    = "/admin/carrier_services.json"
  let args    = ""
  let payload = CarrierServiceSingleton $ f nullObject
  urlPost path args payload >>= decodeCarrierServiceSingleton



{- #DeleteCarrierServicesID -}

deleteCarrierServicesID
  :: IDNumber -> HaskifyM ()
deleteCarrierServicesID n = do
  let path = "/admin/carrier_services/" +$ n $+ ".json"
  let args = ""
  urlDelete path args >> return ()
