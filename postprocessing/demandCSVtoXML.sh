#!/usr/bin/env bash
echo $BASH_VERSION

# change to the directory this script is located in
cd "$(dirname "$0")"

# default values should be false
CSV=false
INPUT=false
OUTPUT=false

# Display help
function show_help()
{
  echo "USAGE: demandCSVtoXML.sh [OPTIONS]... "
  echo "This script creates an xml document from a csv"
  echo "OPTIONS are:"
  echo "  -h, --help          | Display this help message"
  echo "  -i, --input=INPUT   | Location of plans.csv"
  echo "  -o, --output=OUTPUT | Output location of plans.xml"
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
  echo 'No location supplied for plans csv'
  echo 'E.g.: output/example/plans.csv'
  show_help
  exit
fi

if [ "$OUTPUT" = false ] ; then
  echo 'No input location supplied for plans.xml'
  echo 'E.g.: output/example/plans.xml'
  show_help
  exit
fi


# INPUT="exampleXml/plan_small.csv"
# OUTPUT="exampleXml/plan_small2.xml"

CURRENT_ID="-1"
echo '<?xml version="1.0" encoding="utf-8"?>' > $OUTPUT
echo '<!DOCTYPE population SYSTEM "http://www.matsim.org/files/dtd/population_v6.dtd">' >> $OUTPUT
tail -n +2 $INPUT | while IFS=$',' read -r -a LINE
do
  # if we're on a new person
  if [ "$CURRENT_ID" != "${LINE[0]}" ] ; then
    if [ "$CURRENT_ID" != "-1" ] ; then
      echo '  </plan>' >> $OUTPUT
      echo '</person>' >> $OUTPUT
    fi
    echo '<person id="'${LINE[0]}'">' >> $OUTPUT
    echo '  <plan selected="yes">' >> $OUTPUT
    CURRENT_ID="${LINE[0]}"
  fi
  # if it's an activity line
  if [ "${#LINE[@]}" = 5 ] ; then
    echo '    <activity type="'${LINE[1]}'" x="'${LINE[2]}'" y="'${LINE[3]}'" end_time="'${LINE[4]}'"/>' >> $OUTPUT
  else # if it's a leg line
    echo '    <leg mode="'${LINE[5]}'"/>' >> $OUTPUT
  fi
done
echo '  </plan>' >> $OUTPUT
echo '</person>' >> $OUTPUT
echo '</population>' >> $OUTPUT


