#!/bin/bash

SCHEMA="schema/DataFields.txt"
OUT="temp/DataFields.hs"

echo "{-# LANGUAGE OverloadedStrings #-}" > $OUT
echo >> $OUT
echo "module Haskify.DataFields where" >> $OUT
echo >> $OUT
echo "import Data.Text" >> $OUT
echo "import Haskify.Types" >> $OUT
echo "import Haskify.HaskifyM" >> $OUT
echo "import Data.Aeson (Object)" >> $OUT
echo >> $OUT

cat $SCHEMA | awk -F':' '{print "class Has" $1 " t where"; print "  the" $1 "Maybe :: t -> Maybe " $2; print "  set" $1 " :: " $2 " -> t -> t"; print ""; print "  the" $1 " :: t -> HaskifyM " $2; print "  the" $1 " x = case the" $1 "Maybe x of"; print "    Nothing -> fieldDNE \"" $1 "\""; print "    Just y -> return y"; print ""}' >> $OUT
