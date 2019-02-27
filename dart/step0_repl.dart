import 'dart:io';

String READ(String x) => x;

String EVAL(String x) => x;

String PRINT(String x) => x;

String rep(String x) => PRINT(EVAL(READ(x)));

const prompt = 'user> ';
main() {
  while (true) {
    stdout.write(prompt);
    var input = stdin.readLineSync();
    if (input == null) return;
    var output = rep(input);
    stdout.writeln(output);
  }
}
