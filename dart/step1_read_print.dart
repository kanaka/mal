#!/usr/bin/env dart

library mal.step1_read_print;

import "dart:io";
import "types.dart";
import "reader.dart" as reader;
import "printer.dart" as printer;

MalType READ(String str) {
  return reader.read_str(str);
}

MalType EVAL(MalType ast, String env) {
  return ast;
}

void PRINT(MalType exp) {
  stdout.writeln(printer.pr_str(exp));
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
        // Control signal or EOF then break from REPL.
        break;
      }

      rep(line);
    } on Exception catch (ex) {
      stdout.writeln("Error: ${ex.message}");
      break;
    } on StateError catch (ex) {
      stdout.writeln("Error: ${ex.message}");
      break;
    }
  }
}
