#!/bin/bash

# change to the directory this script is located in
cd "$(dirname "$0")"

# the postgres database name and osm file location
INPUT=false
OUTPUT=false

# Display help
function show_help()
{
  echo "USAGE: parseEvents.sh [OPTIONS]... "
  echo "This script creates a gzipped csv from output_events.xml.gz"
  echo "OPTIONS are:"
  echo "  -h, --help          | Display this help message"
  echo "  -i, --input=INPUT   | Location of output_events.xml.gz"
  echo "  -o, --output=OUTPUT | Output location of events.csv"
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
  echo 'No input location supplied for output_events.xml'
  echo 'E.g.: output/example/output_events.xml.gz'
  show_help
  exit
fi

if [ "$OUTPUT" = false ] ; then
  echo 'No location supplied for output csv'
  echo 'E.g.: output/example/output_events.csv.gz'
  show_help
  exit
fi





# writing the header
echo 'time;type;person;link;actType;legMode;vehicle;networkMode;relativePosition;distance;mode;x;y;driverId;vehicleId;transitLineId;transitRouteId;departureId;facility;delay;agent;atStop;destinationStop' | gzip > "$OUTPUT"
# parsing the xml and writing to file
xmlstarlet \
  sel -T -t -m /events/event -v \
  "concat(@time,';',@type,';',@person,';',@link,';',@actType,';',@legMode,';', \
  @vehicle,';',@networkMode,';',@relativePosition,';',@distance,';',@mode,';', \
  @x,';',@y,';',@driverId,';',@vehicleId,';',@transitLineId,';',               \
  @transitRouteId,';',@departureId,';',@facility,';',@delay,';',@agent,';',    \
  @atStop,';',@destinationStop)" \
  -n "$INPUT" | gzip >> "$OUTPUT"

