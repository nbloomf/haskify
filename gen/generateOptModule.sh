#!/bin/bash

SCHEMA="schema/opt/$1.txt"
OUT="temp/Opt/$1.hs"

echo $1 >> temp/OptImport.txt

echo "{-# LANGUAGE OverloadedStrings #-}" > $OUT
echo "{-# LANGUAGE RecordWildCards   #-}" >> $OUT
echo >> $OUT
echo $1 | awk '{print "module Haskify.Opt." $1 " (" $1 "()) where"}' >> $OUT
echo >> $OUT
echo "import Data.Text (Text, intercalate, pack, concat)" >> $OUT
echo "import Data.Aeson" >> $OUT
echo "import Data.Time.Clock (UTCTime)" >> $OUT
echo >> $OUT
echo "import Haskify.Types" >> $OUT
echo >> $OUT

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^PublishedStatus$'); then
  echo "import Haskify.Value.PublishedStatus" >> $OUT; fi

if (cut -d ":" -f 2 $SCHEMA | tr -d "[]" | grep --quiet '^ValueType$'); then
  echo "import Haskify.Value.ValueType" >> $OUT; fi

echo >> $OUT

# Data Definition
echo "data" $1 "=" $1 >> $OUT
head -n +1 $SCHEMA | awk -F':' '{print " { " $1 ":: Maybe " $2}' >> $OUT
tail -n +2 $SCHEMA | awk -F':' '{print " , " $1 ":: Maybe " $2}' >> $OUT
echo " } deriving Show" >> $OUT
echo >> $OUT

# Empty
echo $1 | awk '{print "instance Opt " $1 " where"}' >> $OUT
echo $1 | awk '{print "  emptyOpt = " $1}' >> $OUT
head -n +1 $SCHEMA | awk -F':' '{print "    { " $1 "= Nothing"}' >> $OUT
tail -n +2 $SCHEMA | awk -F':' '{print "    , " $1 "= Nothing"}' >> $OUT
echo "    }" >> $OUT
echo >> $OUT

# ToURLArgs Instance
echo "instance ToURLArgs" $1 "where" >> $OUT
echo $1 | awk '{print "  toURLArgs " $1 "{..} = intercalate \"&amp;\" $ filter (/= \"\")"}' >> $OUT
head -n +1 $SCHEMA | tr -d " " | awk -F':' '{print "    [ case " $1 " of"; print "        Nothing -> \"\""; print "        Just x  -> Data.Text.concat [\"" $1 "=\", pack $ show x]"}' >> $OUT
tail -n +2 $SCHEMA | tr -d " " | awk -F':' '{print "    , case " $1 " of"; print "        Nothing -> \"\""; print "        Just x  -> Data.Text.concat [\"" $1 "=\", pack $ show x]"}' >> $OUT
echo "    ]" >> $OUT
echo >> $OUT



# created_at_min
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^created_at_min$'; } then
  echo "instance HasOptCreatedAtMin" $1 "where" >> $OUT
  echo "  optCreatedAtMin x y = y { created_at_min = Just x }" >> $OUT
  echo "" >> $OUT
fi

# created_at_max
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^created_at_max$'; } then
  echo "instance HasOptCreatedAtMax" $1 "where" >> $OUT
  echo "  optCreatedAtMax x y = y { created_at_max = Just x }" >> $OUT
  echo "" >> $OUT
fi

# limit
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^limit$'; } then
  echo "instance HasOptLimit" $1 "where" >> $OUT
  echo "  optLimit x y = y { limit = Just x }" >> $OUT
  echo "" >> $OUT
fi

# page
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^page$'; } then
  echo "instance HasOptPage" $1 "where" >> $OUT
  echo "  optPage x y = y { page = Just x }" >> $OUT
  echo "" >> $OUT
fi

# since_id
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^since_id$'; } then
  echo "instance HasOptSinceID" $1 "where" >> $OUT
  echo "  optSinceID x y = y { since_id = Just x }" >> $OUT
  echo "" >> $OUT
fi

# updated_at_min
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^updated_at_min$'; } then
  echo "instance HasOptUpdatedAtMin" $1 "where" >> $OUT
  echo "  optUpdatedAtMin x y = y { updated_at_min = Just x }" >> $OUT
  echo "" >> $OUT
fi

# updated_at_max
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^updated_at_max$'; } then
  echo "instance HasOptUpdatedAtMax" $1 "where" >> $OUT
  echo "  optUpdatedAtMax x y = y { updated_at_max = Just x }" >> $OUT
  echo "" >> $OUT
fi

# vendor
if { cut -d ':' -f 1 $SCHEMA | tr -d ' ' | grep --quiet '^vendor$'; } then
  echo "instance HasOptVendor" $1 "where" >> $OUT
  echo "  optVendor x y = y { vendor = Just x }" >> $OUT
  echo "" >> $OUT
fi
