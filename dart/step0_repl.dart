#!/usr/bin/env dart

library mal.step0_repl;

import "dart:io";

String READ(String str) {
  return str;
}

String EVAL(String ast, String env) {
  return ast;
}

void PRINT(String exp) {
  print(exp);
}

void rep(String str) {
  PRINT(EVAL(READ(str), ""));
}

void main(List<String> args) {
  String line;

  while (true) {
    stdout.write("user> ");
    try {
      line = stdin.readLineSync();
      if (line == null) {
        // Control signal or EOF then break from repl.
        break;
      }

      rep(line);
    } catch (ex) {
      print(ex);
      break;
    }
  }
}
