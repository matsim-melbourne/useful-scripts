#!/usr/bin/env bash

FILE="$1""_bicycle"

zcat $1 |
sed -e "s/bike/bicycle/g" |
gzip >> $FILE
