#!/usr/bin/env bash

FILE="netwalk_""$1"

zcat $1 |
sed -e "s/walk/netwalk/g" |
gzip >> $FILE
