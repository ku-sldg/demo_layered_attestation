#!/usr/bin/sh

DIR3=d3
DIR4=d4
FILE=out.txt

cat $DIR3/$FILE > $DIR4/$FILE
echo ":d34:" >> $DIR4/$FILE
unlink $DIR3/$FILE
