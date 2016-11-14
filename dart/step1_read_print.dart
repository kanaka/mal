import 'dart:io';

import 'printer.dart' as printer;
import 'reader.dart' as reader;
import 'types.dart';

MalType READ(String x) => reader.read_str(x);

MalType EVAL(MalType x) => x;

String PRINT(MalType x) => printer.pr_str(x);

String rep(String x) {
  var parsed;
  try {
    parsed = READ(x);
  } on reader.ParseException catch (e) {
    return e.message;
  }
  return PRINT(EVAL(parsed));
}

const prompt = 'user> ';
main() {
  while (true) {
    stdout.write(prompt);
    var input = stdin.readLineSync();
    if (input == null) return;
    var output;
    try {
      output = rep(input);
    } on reader.NoInputException {
      continue;
    }
    stdout.writeln(output);
  }
}
