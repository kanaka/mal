#include <stdio.h>
#include <stdlib.h>

#include "linked_list.h"


int main(int argc, char** argv) {

  /* Greeting message */
  //  puts("Test linked list\n");

  char* data1 = "test data 1";
  char* data2 = "test data 2";
  char* data3 = "test data 3";

  printf("Initial data\n");
  printf("%s %s\n", "data1 =", data1);
  printf("%s %s\n", "data2 =", data2);
  printf("%s %s\n", "data3 =", data3);

  printf("Making a list with data1\n");
  list my_list= list_make(data1);
  printf("List data: %s\n", (char*)my_list->data);
  printf("List next: %p\n", my_list->next);


  printf("Pushing data\n");
  my_list = list_push(my_list, data2);
  my_list = list_push(my_list, data3);
  printf("Pushing %p\n", data2);
  printf("Pushing %p\n", data3);

  printf("Number of elements: %ld\n", list_count(my_list));

  printf("Reversing list\n");
  my_list = list_reverse(my_list);
  printf("Number of elements: %ld\n", list_count(my_list));

  printf("Popping data\n");
  printf("%s %s\n", "data1 =", (char*)list_peek(my_list));
  my_list = list_pop(my_list);
  printf("%s %s\n", "data2 =", (char*)list_peek(my_list));
  my_list = list_pop(my_list);
  printf("%s %s\n", "data3 =", (char*)list_peek(my_list));
  my_list = list_pop(my_list);

  if (my_list) {
    printf("List not empty!\n");
  } else {
    printf("List empty\n");
  }

  printf("Making a list with a b c\n");
  list my_list_1 = list_make("a");
  my_list_1 = list_push(my_list_1, "b");
  my_list_1 = list_push(my_list_1, "c");
  my_list_1 = list_reverse(my_list_1);

  printf("Making a list with 1 2 3\n");
  list my_list_2 = list_make("1");
  my_list_2 = list_push(my_list_2, "2");
  my_list_2 = list_push(my_list_2, "3");
  my_list_2 = list_reverse(my_list_2);

  printf("concatenating lists to get (a b c 1 2 3)\n");

  list my_concatenated_list = list_concatenate(my_list_1, my_list_2);

  printf("Popping data\n");
  printf("(%s ", (char*)list_peek(my_concatenated_list));
  my_concatenated_list = list_pop(my_concatenated_list);
  printf("%s ", (char*)list_peek(my_concatenated_list));
  my_concatenated_list = list_pop(my_concatenated_list);
  printf("%s ", (char*)list_peek(my_concatenated_list));
  my_concatenated_list = list_pop(my_concatenated_list);
  printf("%s ", (char*)list_peek(my_concatenated_list));
  my_concatenated_list = list_pop(my_concatenated_list);
  printf("%s ", (char*)list_peek(my_concatenated_list));
  my_concatenated_list = list_pop(my_concatenated_list);
  printf("%s)", (char*)list_peek(my_concatenated_list));
  my_concatenated_list = list_pop(my_concatenated_list);

  return 0;
}
