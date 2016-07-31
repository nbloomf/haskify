#!/bin/bash

# This script generates data modules. Takes the NAME
# of the data type as a parameter. Expects to find 
# name:value pairs in NAME.txt (the schema). Writes
# to NAME.hs

SCHEMA="schema/data/$1.txt"
OUT="temp/Data/$1.hs"

echo $1 >> temp/DataImport.txt

#-------------#
# Frontmatter #
#-------------#

echo "{-# LANGUAGE OverloadedStrings #-}" > $OUT
echo "{-# LANGUAGE RecordWildCards   #-}" >> $OUT
echo >> $OUT
echo $1 | awk '{print "module Haskify.Data." $1 " ("}' >> $OUT
echo $1 | awk '{print "  " $1 "()"}' >> $OUT
echo $1 | awk '{print "  , Has" $1 ", the" $1 "Maybe, set" $1 ", the" $1 ", Has" $1 "s, the" $1 "sMaybe, set" $1 "s, the" $1 "s"}' >> $OUT

if [ "$1" == "CustomerAddress" ]; then
  echo ", HasDefaultAddress, theDefaultAddressMaybe, setDefaultAddress, theDefaultAddress" >> $OUT
fi

if [ "$1" == "CarrierServiceLocation" ]; then
  echo ", HasOrigin, theOriginMaybe, setOrigin, theOrigin" >> $OUT
  echo ", HasDestination, theDestinationMaybe, setDestination, theDestination" >> $OUT
fi

echo ") where" >> $OUT
echo >> $OUT
echo "import Prelude hiding (id, zip)" >> $OUT
echo >> $OUT
echo "import Data.Text (Text, pack, concat, intercalate)" >> $OUT
echo "import Data.Aeson" >> $OUT
echo "import Data.Time.Clock (UTCTime)" >> $OUT
echo >> $OUT
echo "import Haskify.Types" >> $OUT
echo "import Haskify.DataFields" >> $OUT
echo "import Haskify.JSON" >> $OUT
echo "import Haskify.HaskifyM" >> $OUT



#--------------#
# Data Imports #
#--------------#

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ApplicationChargeStatus$'); then
  echo "import Haskify.Value.ApplicationChargeStatus" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ApplicationChargeTest$'); then
  echo "import Haskify.Value.ApplicationChargeTest" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^CarrierServiceType$'); then
  echo "import Haskify.Value.CarrierServiceType" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Column$'); then
  echo "import Haskify.Value.Column" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Commentable$'); then
  echo "import Haskify.Value.Commentable" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^CustomerState$'); then
  echo "import Haskify.Value.CustomerState" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^DisplayScope$'); then
  echo "import Haskify.Value.DisplayScope" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^DOMEvent$'); then
  echo "import Haskify.Value.DOMEvent" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Format$'); then
  echo "import Haskify.Value.Format" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^InventoryManagement$'); then
  echo "import Haskify.Value.InventoryManagement" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^InventoryPolicy$'); then
  echo "import Haskify.Value.InventoryPolicy" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^PaymentDetails$'); then
  echo "import Haskify.Value.PaymentDetails" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^PublishedScope$'); then
  echo "import Haskify.Value.PublishedScope" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^PublishedStatus$'); then
  echo "import Haskify.Value.PublishedStatus" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Relation$'); then
  echo "import Haskify.Value.Relation" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^SortOrder$'); then
  echo "import Haskify.Value.SortOrder" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ThemeRole$'); then
  echo "import Haskify.Value.ThemeRole" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^TransactionErrorCode$'); then
  echo "import Haskify.Value.TransactionErrorCode" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^TransactionReceipt$'); then
  echo "import Haskify.Value.TransactionReceipt" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^TransactionStatus$'); then
  echo "import Haskify.Value.TransactionStatus" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^TransactionKind$'); then
  echo "import Haskify.Value.TransactionKind" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ValueType$'); then
  echo "import Haskify.Value.ValueType" >> $OUT; fi



if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Blog$'); then
  echo "import Haskify.Data.Blog" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^CustomerAddress$'); then
  echo "import Haskify.Data.CustomerAddress" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^CarrierServiceLocation$'); then
  echo "import Haskify.Data.CarrierServiceLocation" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^CarrierServiceItem$'); then
  echo "import Haskify.Data.CarrierServiceItem" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Image$'); then
  echo "import Haskify.Data.Image" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Metafield$'); then
  echo "import Haskify.Data.Metafield" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ProductVariant$'); then
  echo "import Haskify.Data.ProductVariant" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ProductImage$'); then
  echo "import Haskify.Data.ProductImage" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Province$'); then
  echo "import Haskify.Data.Province" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^Rule$'); then
  echo "import Haskify.Data.Rule" >> $OUT; fi

echo >> $OUT



#------------#
# Definition #
#------------#

echo "data" $1 "=" $1 >> $OUT
head -n +1 $SCHEMA | awk -F':' '{print " { " $1 ":: Maybe " $2}' >> $OUT
tail -n +2 $SCHEMA | awk -F':' '{print " , " $1 ":: Maybe " $2}' >> $OUT
echo " } deriving (Eq, Show)" >> $OUT
echo >> $OUT

echo $1 | awk '{print "class Has" $1 " t where"}' >> $OUT
echo $1 | awk '{print "  the" $1 "Maybe :: t -> Maybe " $1}' >> $OUT
echo $1 | awk '{print "  set" $1 " :: " $1 " -> t -> t"}' >> $OUT
echo >> $OUT
echo $1 | awk '{print "  the" $1 " :: t -> HaskifyM " $1}' >> $OUT
echo $1 | awk '{print "  the" $1 " x = case the" $1 "Maybe x of"}' >> $OUT
echo $1 | awk '{print "    Nothing -> fieldDNE \"" $1 "\""}' >> $OUT
echo $1 | awk '{print "    Just y -> return y"}' >> $OUT
echo >> $OUT

echo $1 | awk '{print "class Has" $1 "s t where"}' >> $OUT
echo $1 | awk '{print "  the" $1 "sMaybe :: t -> Maybe [" $1 "]"}' >> $OUT
echo $1 | awk '{print "  set" $1 "s :: [" $1 "] -> t -> t"}' >> $OUT
echo >> $OUT
echo $1 | awk '{print "  the" $1 "s :: t -> HaskifyM [" $1 "]"}' >> $OUT
echo $1 | awk '{print "  the" $1 "s x = case the" $1 "sMaybe x of"}' >> $OUT
echo $1 | awk '{print "    Nothing -> fieldDNE \"" $1 "\""}' >> $OUT
echo $1 | awk '{print "    Just y -> return y"}' >> $OUT
echo >> $OUT


if [ "$1" == 'CustomerAddress' ]; then
  echo $1 | awk '{print "class HasDefaultAddress t where"}' >> $OUT
  echo $1 | awk '{print "  theDefaultAddressMaybe :: t -> Maybe CustomerAddress"}' >> $OUT
  echo $1 | awk '{print "  setDefaultAddress :: CustomerAddress -> t -> t"}' >> $OUT
  echo >> $OUT
  echo $1 | awk '{print "  theDefaultAddress :: t -> HaskifyM CustomerAddress"}' >> $OUT
  echo $1 | awk '{print "  theDefaultAddress x = case theDefaultAddressMaybe x of"}' >> $OUT
  echo $1 | awk '{print "    Nothing -> fieldDNE \"DefaultAddress\""}' >> $OUT
  echo $1 | awk '{print "    Just y -> return y"}' >> $OUT
  echo >> $OUT
fi


if [ "$1" == 'CarrierServiceLocation' ]; then
  echo $1 | awk '{print "class HasOrigin t where"}' >> $OUT
  echo $1 | awk '{print "  theOriginMaybe :: t -> Maybe CarrierServiceLocation"}' >> $OUT
  echo $1 | awk '{print "  setOrigin :: CarrierServiceLocation -> t -> t"}' >> $OUT
  echo >> $OUT
  echo $1 | awk '{print "  theOrigin :: t -> HaskifyM CarrierServiceLocation"}' >> $OUT
  echo $1 | awk '{print "  theOrigin x = case theOriginMaybe x of"}' >> $OUT
  echo $1 | awk '{print "    Nothing -> fieldDNE \"Origin\""}' >> $OUT
  echo $1 | awk '{print "    Just y -> return y"}' >> $OUT
  echo >> $OUT

  echo $1 | awk '{print "class HasDestination t where"}' >> $OUT
  echo $1 | awk '{print "  theDestinationMaybe :: t -> Maybe CarrierServiceLocation"}' >> $OUT
  echo $1 | awk '{print "  setDestination :: CarrierServiceLocation -> t -> t"}' >> $OUT
  echo >> $OUT
  echo $1 | awk '{print "  theDestination :: t -> HaskifyM CarrierServiceLocation"}' >> $OUT
  echo $1 | awk '{print "  theDestination x = case theDestinationMaybe x of"}' >> $OUT
  echo $1 | awk '{print "    Nothing -> fieldDNE \"Destination\""}' >> $OUT
  echo $1 | awk '{print "    Just y -> return y"}' >> $OUT
  echo >> $OUT
fi


#-----------#
# Instances #
#-----------#

# NullObject Instance
echo "instance NullObject" $1 "where" >> $OUT
echo "  nullObject =" $1 >> $OUT
head -n +1 $SCHEMA | awk -F':' '{print "    { " $1 "= Nothing"}' >> $OUT
tail -n +2 $SCHEMA | awk -F':' '{print "    , " $1 "= Nothing"}' >> $OUT
echo "   }" >> $OUT
echo >> $OUT

# FromJSON Instance
echo "instance FromJSON" $1 "where" >> $OUT
echo $1 | awk '{print "  parseJSON = withObject " "\"" $1 "\" $ \\o -> do"}' >> $OUT
awk -F':' '{printf "    " $1 "<- o .:? \""; gsub(/ /, "", $1); print $1 "\""}' $SCHEMA >> $OUT
echo $1 | awk '{print "    return " $1 "{..}" }' >> $OUT
echo -e "\n" >> $OUT

# ToJSON Instance
echo "instance ToJSON" $1 "where" >> $OUT
echo $1 | awk '{print "  toJSON " $1 "{..} = objectSansNull"}' >> $OUT
head -n +1 $SCHEMA | sed "s/^\([^[:space:]]*\)\([[:space:]]*\)/\1\2:\"\1\"\2:/" | \
  awk -F':' '{gsub(/ /, "", $1); print "    [ " $2 ".= " $1}' >> $OUT
tail -n +2 $SCHEMA | sed "s/^\([^[:space:]]*\)\([[:space:]]*\)/\1\2:\"\1\"\2:/" | \
  awk -F':' '{gsub(/ /, "", $1); print "    , " $2 ".= " $1}' >> $OUT
echo "    ]" >> $OUT
echo -e "\n" >> $OUT

# ToURLArgs Instance
echo "instance ToURLArgs" $1 "where" >> $OUT
echo $1 | awk '{print "  toURLArgs " $1 "{..} = intercalate \"&\" $ filter (/= \"\")"}' >> $OUT
head -n +1 $SCHEMA | tr -d " " | sed "s/$/:$2/" | awk -F':' '{print "    [ case " $1 " of"; print "        Nothing -> \"\""; print "        Just x  -> Data.Text.concat [\"" $4 "[" $1 "]=\", pack $ show x]"}' >> $OUT
tail -n +2 $SCHEMA | tr -d " " | sed "s/$/:$2/" | awk -F':' '{print "    , case " $1 " of"; print "        Nothing -> \"\""; print "        Just x  -> Data.Text.concat [\"" $4 "[" $1 "]=\", pack $ show x]"}' >> $OUT
echo "    ]" >> $OUT
echo >> $OUT



# Getters and Setters
sed "s/$/:$1/" $SCHEMA | awk -F':' '{print "instance Has" $3 " " $4 " where"; print "  the" $3 "Maybe = " $1; gsub(/ /, "", $1); print "  set" $3 " x y = y { " $1 " = Just x }"; print ""}' >> $OUT
