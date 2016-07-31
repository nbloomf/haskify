#!/bin/bash

# This script generates value modules. Takes the NAME
# of the data type as a parameter. Expects to find 
# values value/NAME.txt (the schema). Writes
# to Value/NAME.hs

SCHEMA="schema/value/$1.txt"
OUT="temp/Value/$1.hs"

echo $1 >> temp/ValueImport.txt

# Frontmatter
echo "{-# LANGUAGE OverloadedStrings #-}" > $OUT
echo >> $OUT
echo $1 | awk '{print "module Haskify.Value." $1 " (" $1 "(..), Has" $1 ", the" $1 "Maybe, the" $1 ", set" $1 ") where"}' >> $OUT
echo >> $OUT
echo "import Data.Text (Text)" >> $OUT
echo "import Data.Aeson" >> $OUT
echo "import Haskify.HaskifyM" >> $OUT
echo >> $OUT

# Data Definition
echo "data $1" >> $OUT
head -n +1 $SCHEMA | sed "s/^/$1/" | awk -F':' '{print " = " $1}' >> $OUT
tail -n +2 $SCHEMA | sed "s/^/$1/" | awk -F':' '{print " | " $1}' >> $OUT
echo " deriving Eq" >> $OUT
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

# Show Instance
echo "instance Show" $1 "where" >> $OUT
sed "s/^\(.*\)$/$1\1:\1/" $SCHEMA | \
  awk -F':' '{print "  show " $1 " = \"" $2 "\""}' >> $OUT
echo >> $OUT

# FromJSON Instance
echo "instance FromJSON" $1 "where" >> $OUT
echo $1 | awk '{print "  parseJSON = withText " "\"" $1 "\" $ \\s -> case s of"}' >> $OUT
sed "s/^\(.*\)$/$1\1:\1/" $SCHEMA | \
  awk -F':' '{print "    \"" $2 "\" -> return " $1}' >> $OUT
cut -d':' -f 2 $SCHEMA | tr '\n' ' ' | awk '{print "    _ -> fail \"expecting: " $0 "\""}' >> $OUT
echo >> $OUT

# ToJSON Instance
echo "instance ToJSON" $1 "where" >> $OUT
sed "s/^\(.*\)$/$1\1:\1/" $SCHEMA | \
  awk -F':' '{print "  toJSON " $1 " = String \"" $2 "\""}' >> $OUT
echo >> $OUT
