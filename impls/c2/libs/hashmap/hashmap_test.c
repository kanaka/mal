#include <stdio.h>
#include <stdlib.h>

#include "hashmap.h"


int main(int argc, char** argv) {

  /* Greeting message */
  puts("Test hashmap\n");

  char* key1 = ":key 1";
  char* key2 = ":key 2";
  char* key3 = ":key 3";
  char* data1 = "test data 1";
  char* data2 = "test data 2";
  char* data3 = "test data 3";

  printf("Initial data\n");
  printf("%s %s\n", "key1 =", key1);
  printf("%s %s\n", "key2 =", key2);
  printf("%s %s\n", "key3 =", key3);
  printf("%s %s\n", "data1 =", data1);
  printf("%s %s\n", "data2 =", data2);
  printf("%s %s\n", "data3 =", data3);


  printf("Making a hashmap with key1, data1\n");
  hashmap my_hashmap= hashmap_make(key1, data1);
  printf("hashmap key: %s\n", (char*)my_hashmap->data);
  printf("hashmap data: %s\n", (char*)my_hashmap->next->data);

  printf("Pushing data\n");
  my_hashmap = hashmap_put(my_hashmap, key2, data2);
  my_hashmap = hashmap_put(my_hashmap, key3, data3);

  printf("Pushing %p\n", data2);
  printf("Pushing %p\n", data3);

  printf("Number of elements: %ld\n", list_count(my_hashmap));

  printf("fetching data by key\n");
  printf("%s %s\n", "data3 =", (char*)hashmap_get(my_hashmap, key3));
  printf("%s %s\n", "data1 =", (char*)hashmap_get(my_hashmap, key1));
  printf("%s %s\n", "data2 =", (char*)hashmap_get(my_hashmap, key2));

  return 0;
}
