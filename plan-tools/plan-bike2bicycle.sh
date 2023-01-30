#!/usr/bin/env bash

FILE="bicycle_""$1"

zcat $1 |
sed -e "s/bike/bicycle/g" |
gzip >> $FILE
