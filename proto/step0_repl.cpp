// Proto Language
//
// Implementation of a Lisp dialect from the MAL project.
//     https://github.com/kanaka/mal/blob/master/process/guide.md
//
// Copyright 2018 Patrick Laukaitis

#include <iostream>
#include <readline/history.h>
#include <readline/readline.h>
#include <string>

std::string READ(const std::string & str);
std::string EVAL(const std::string & str);
std::string PRINT(const std::string & str);
void rep(const std::string & input);
void PrintPrompt();

int main(int argc, char ** argv)
{
  while(1)
  {
    char * input;
    while((input = readline("user> ")) != nullptr)
    {
      std::string line(input);
      if(line.size() > 0)
      {
        add_history(input);
      }

      // readline will malloc a new buffer on each call
      free(input);

      rep(line);
    }
  }

  return 0;
}

std::string READ(const std::string & str)
{
  return str;
}

std::string EVAL(const std::string & str)
{
  return str;
}

std::string PRINT(const std::string & str)
{
  std::cout << str << std::endl;
  return str;
}

void rep(const std::string & input)
{
  PRINT( EVAL ( READ(input)));
}



