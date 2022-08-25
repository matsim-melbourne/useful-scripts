#!/usr/bin/env bash

FILE="$1""_netwalk"

zcat $1 |
sed -e "s/walk/_netwalk/g" |
gzip >> $FILE
