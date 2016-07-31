#!/bin/bash

SCHEMA="temp/DataImport.txt"
OUT="temp/Data.hs"

echo "module Haskify.Data (" > $OUT

head -n +1 $SCHEMA | awk '{print "    module Haskify.Data." $1}' >> $OUT
tail -n +2 $SCHEMA | awk '{print "  , module Haskify.Data." $1}' >> $OUT

echo ") where" >> $OUT
echo >> $OUT

awk '{print "import Haskify.Data." $1}' $SCHEMA >> $OUT
