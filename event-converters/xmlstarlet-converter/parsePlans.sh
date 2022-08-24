#!/bin/bash

# change to the directory this script is located in
cd "$(dirname "$0")"

# the postgres database name and osm file location
INPUT=false
OUTPUT=false

# Display help
function show_help()
{
  echo "USAGE: parsePlans.sh [OPTIONS]... "
  echo "This script creates a gzipped csv from output_plans.xml.gz"
  echo "OPTIONS are:"
  echo "  -h, --help          | Display this help message"
  echo "  -i, --input=INPUT   | Location of output_plans.xml.gz"
  echo "  -o, --output=OUTPUT | Output location of plans.csv"
}

# Parse command line arguments
for arg in "$@"
do
  case "$arg" in
    -h   | --help      )  show_help; exit 0 ;;
    -d=* | --input=*   )  INPUT="${arg#*=}" ;;
    -o=* | --output=*  )  OUTPUT="${arg#*=}" ;;
    *) echo "Invalid option: $arg" >&2; show_help; exit 1 ;;
  esac
done

if [ "$INPUT" = false ] ; then
  echo 'No input location supplied for output_plans.xml'
  echo 'E.g.: output/example/output_plans.xml.gz'
  show_help
  exit
fi

if [ "$OUTPUT" = false ] ; then
  echo 'No location supplied for output_plans csv'
  echo 'E.g.: output/example/output_plans.csv.gz'
  show_help
  exit
fi



# writing the header
#   person      | person/activity      | person/leg
echo 'person_id;score;type;link;x;y;end_time;mode;dep_time;trav_time;routing_mode;route_type;start_link;end_link;route_trav_time;distance;vehicle_ref_id;links' | gzip > "$OUTPUT"
# parsing the xml and writing to file:
# 1. iterate through each plan (-m population/person/plan)
# 2. only take the plans that are used (--if "@selected='yes'")
# 3. iterate through each plan's activity and leg nodes (-m '*')
# 4. convert to csv -v "concat(...)"
# 5. do all this from the input file (-n "$INPUT")
# 6. pipe results to gzipped csv (| gzip >> "$OUTPUT")
xmlstarlet \
  sel -T -t -m population/person/plan --if "@selected='yes'" \
  -m '*' -v \
  "concat(ancestor::person/@id,';',ancestor::plan/@score,';',@type,';',@link,';',\
  @x,';',@y,';',@end_time,';',@mode,';',@dep_time,';',@trav_time,';',\
  attributes/attribute,';',route/@type,';',route/@start_link,';',route/@end_link,';',\
  route/@trav_time,';',route/@distance,';',route/@vehicleRefId,';',route)" \
  -n "$INPUT" | gzip >> "$OUTPUT"

