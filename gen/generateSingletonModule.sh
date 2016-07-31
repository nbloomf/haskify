#!/bin/bash

SCHEMA="schema/Singleton.txt"
OUT="temp/Singleton.hs"

echo "{-# LANGUAGE OverloadedStrings #-}" > $OUT
echo "{-# LANGUAGE RecordWildCards   #-}" >> $OUT
echo >> $OUT
echo "module Haskify.Singleton where" >> $OUT
echo >> $OUT
echo "import Data.Text" >> $OUT
echo "import Data.Aeson" >> $OUT
echo "import Data.ByteString.Lazy (ByteString)" >> $OUT
echo >> $OUT
echo "import Haskify.Data" >> $OUT
echo "import Haskify.HaskifyM" >> $OUT
echo "import Haskify.JSON" >> $OUT
echo "import Network.HTTP.Types.Header" >> $OUT
echo >> $OUT
echo >> $OUT
echo >> $OUT

cat $SCHEMA | awk -F':' '{print "data " $2 "Singleton = " $2 "Singleton"; print " { un" $2 "Singleton :: " $3; print " } deriving Show"; print ""; print "instance FromJSON " $2 "Singleton where"; print "  parseJSON = withObject \"" $1 "\" $ \\o -> do"; print "    x <- o .: \"" $1 "\""; print "    return $ " $2 "Singleton x"; print ""; print "instance ToJSON " $2 "Singleton where"; print "  toJSON " $2 "Singleton{..} = objectSansNull"; print "    [ \"" $1 "\" .= un" $2 "Singleton"; print "    ]"; print ""; print "decode" $2 "Singleton :: ByteString -> HaskifyM " $3; print "decode" $2 "Singleton x = tryDecode x >>= (return . un" $2 "Singleton)"; print ""; print ""; print "";}' >> $OUT
