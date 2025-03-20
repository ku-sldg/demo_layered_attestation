#!/usr/bin/sh

DIR1=d1
FILE=out.txt

touch $DIR1/$FILE
echo ":master:" > $DIR1/$FILE

./p12.sh

./p23.sh

./p34.sh
