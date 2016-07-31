#!/bin/bash

SCHEMA="temp/OptImport.txt"
OUT="temp/Opt.hs"

echo "module Haskify.Opt (" > $OUT

head -n +1 $SCHEMA | awk '{print "    module Haskify.Opt." $1}' >> $OUT
tail -n +2 $SCHEMA | awk '{print "  , module Haskify.Opt." $1}' >> $OUT

echo ") where" >> $OUT
echo >> $OUT

awk '{print "import Haskify.Opt." $1}' $SCHEMA >> $OUT
