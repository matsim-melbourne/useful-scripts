
Make sure to install xmlstarlet:

```bash
sudo apt install xmlstarlet
```



Converting the events and plans to csv (note that ';' is used instead of ','):

```bash
./parseEvents.sh \
  --input='../example-project/output/example (car,bike,pt,tele-walk)/output_events.xml.gz' \
  --output='./output_events.csv.gz'

./parsePlans.sh \
  --input='../example-project/output/example (car,bike,pt,tele-walk)/output_plans.xml.gz' \
  --output='./output_plans.csv.gz'
```

onverting the events and plans to csv (note that ';' is used instead of ','):

```bash
./demandXMLtoCSV.sh \
  --input='plan.xml' \
  --output='output_events.csv'

./demandCSVtoXML.sh \
  --input='output_events.csv' \
  --output='output_events.xml'
```

