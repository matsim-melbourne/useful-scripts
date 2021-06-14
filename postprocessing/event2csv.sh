#!/bin/bash
if [[ $# -eq 0 ]]; then
  events_file=output_events.xml.gz;
else
  events_file=$1
fi

# Departure and Arrival
zcat $events_file | tail -n +3 | head -n -2 |
grep "departure\|arrival" |
cut -d "\"" -f 2,4,6,8,10 |
sed -e "s/\"/,/g"|
sed -e '1i\'$'\n''time,type,person,link,legMode' | cat > person_trip.txt

# left link and entered link
zcat $events_file | tail -n +3 | head -n -2 |
grep "left link\|entered link" |
sed -e "s/left link/left_link/g; s/entered link/entered_link/g " |
cut -d "\"" -f 2,4,6,8 |
sed -e "s/\"/,/g"|
sed -e '1i\'$'\n''time,type,vehicle,link' | cat > trip_links.txt

# vehicle-traffic interaction
zcat $events_file | tail -n +3 | head -n -2 |
grep "vehicle leaves traffic\|vehicle enters traffic" |
sed -e "s/vehicle leaves traffic/vehicle_leaves_traffic/g;" |
sed -e "s/vehicle enters traffic/vehicle_enters_traffic/g;" |
cut -d "\"" -f 2,4,6,8,10,12 |
sed -e "s/\"/,/g"|
sed -e '1i\'$'\n''time,type,person,link,vehicle,networkMode' | cat > vehicle_trip.txt

# Activity-startend actstart
zcat  $events_file | tail -n +3 | head -n -2 |
grep "actstart\|actend" |
cut -d "\"" -f 2,4,6,8,10 |
sed -e "s/\"/,/g"|
sed -e '1i\'$'\n''time,type,person,link,actType' | cat > person_act.txt
