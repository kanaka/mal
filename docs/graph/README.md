# Mal Implementation Stats Graph


## Updating the data

* Install prerequisites:

For ubuntu:
```
sudo apt-get install gh
sudo apt-get golang
```

For macos:
```
brew install gh
brew install go
```

* Create logs dir and enter graph dir:
```
mkdir -p docs/graph/logs
cd docs/graph/logs
```

* Install npm deps
```
npm install
```

* Clone and build loccount:
```
git clone https://gitlab.com/esr/loccount
make -C loccount
```

* Auth with github:
```
gh auth login
```

* Download artifacts from a recent full and successful workflow run:

```
# list workflow runs
$ gh run list --repo kanaka/mal

# Download recent full successful run:
$ gh run download 10598199016 --repo kanaka/mal
```

* Run the [StackOverflow tags
  query](https://data.stackexchange.com/stackoverflow/query/edit/1013465)
  and then download the CSV link:

```
curl https://data.stackexchange.com/stackoverflow/csv/2267200 -o so-tags.csv
```

* Remove/clean all generated files:

```
( cd ../.. && git ls-files --others impls/ | xargs rm )
```

* Download GitHub and StackOverflow data and generate the final
  combined data set:

```
PATH=$PATH:$(pwd)/loccount
time VERBOSE=1 node ./collect_data.js logs/ all_data.json
```
