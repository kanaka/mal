# Mal Implementation Stats Graph


## Updating the data

* Install prerequisites:

```
sudo aptitude install ruby2.3-dev
sudo gem install travis --no-rdoc --no-ri
```

* Download the latest successful travis build (BUILD is the travis
  build number):

```
cd docs/graph

BUILD=1490

for x in $(seq 1 109); do echo ${BUILD}/${x}; mkdir -p logs/${BUILD}; while ! travis logs ${BUILD}.${x} > logs/${BUILD}/${x}; do true; done; done
```

* Run the [StackOverflow tags query](https://data.stackexchange.com/stackoverflow/query/edit/1013465) and update the CSV link:

```
export SO_TAG_CSV_URL=... # from the query page
    E.G.
export SO_TAG_CSV_URL=https://data.stackexchange.com/stackoverflow/csv/1451851
```

* Remove/clean all generated files:

```
make -C ../.. clean
```

* Download GitHub and StackOverflow data and generate the final
  combined data set:

```
PATH=$PATH:~/personal/programming/loccount

npm install
time VERBOSE=1 node ./collect_data.js logs/${BUILD}/ all_data.json
```
