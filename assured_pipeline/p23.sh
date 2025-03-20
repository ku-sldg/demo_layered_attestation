#!/usr/bin/sh

DIR2=d2
DIR3=d3
FILE=out.txt

cat $DIR2/$FILE > $DIR3/$FILE
echo ":d23:" >> $DIR3/$FILE
unlink $DIR2/$FILE
