#!/bin/bash

SCHEMA="temp/ValueImport.txt"
OUT="temp/Value.hs"

echo "module Haskify.Value (" > $OUT

head -n +1 $SCHEMA | awk '{print "    module Haskify.Value." $1}' >> $OUT
tail -n +2 $SCHEMA | awk '{print "  , module Haskify.Value." $1}' >> $OUT

echo ") where" >> $OUT
echo >> $OUT

awk '{print "import Haskify.Value." $1}' $SCHEMA >> $OUT
