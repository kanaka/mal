## Counting languages, implementations, and runtimes/MODES

```
# languages
$ egrep -v "\-mal\>|IMPL: mal,.*nim" IMPLS.yml | grep -o "\<IMPL: [^,}]*" | awk '{print $2}' | sort | egrep -v "\.2$|swift[0-9]" | uniq | wc
      89      89     526

# implementations
$ egrep -v "\-mal\>|IMPL: mal,.*nim" IMPLS.yml | grep -o "\<IMPL: [^,}]*" | awk '{print $2}' | sort | uniq | wc
      95      95     564

# runtimes/MODES
$ egrep -v "\-mal\>|IMPL: mal,.*nim" IMPLS.yml | grep -o "\<IMPL: [^,}]*" | awk '{print $2}' | sort | wc
     117     117     681
```
