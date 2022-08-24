#!/usr/bin/env bash

FILE="v6_""$1"


zcat $1 |
sed -e "s/population_v5.dtd/population_v6.dtd/g" |
sed -e "s/bike/bicycle/g" |
sed -e "s/act /activity /g" |
sed -e 's/<route.*\/route>//g' |
sed -e 's/link.*x/x/g' |
sed -e 's/ end_time/ z\="10" end_time/g;' | sed -e 's/ max_dur/ z="10" max_dur/g' |
sed '/z/! s/\/>/z="10" \/>/g' |
sed 's/ sex="/>\n            <attributes>\n                <attribute name="sex" class="java.lang.Boolean">/g' |
sed 's/" age="/<\/attribute>\n                <attribute name="age" class="java.lang.Boolean">/g' |
sed 's/" car_avail="/<\/attribute>\n                <attribute name="car_avail" class="java.lang.Boolean">/g' |
sed 's/" employed="/<\/attribute>\n                <attribute name="employed" class="java.lang.Boolean">/g' |
sed 's/Boolean">no">/Boolean">no<\/attribute>\n            <\/attributes>\n/g' |
sed 's/Boolean">yes">/Boolean">yes<\/attribute>\n            <\/attributes>\n/g' |
gzip >> $FILE
