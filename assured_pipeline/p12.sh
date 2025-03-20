#!/usr/bin/sh

DIR1=d1
DIR2=d2
FILE=out.txt

cat $DIR1/$FILE > $DIR2/$FILE
echo ":d12:" >> $DIR2/$FILE
unlink $DIR1/$FILE
