library test_helpers;

import "../types.dart";

typedef MalType TestEval(String input);

void expectResults(TestEval testEval, String input, String expectedOutput, {isFailing: false}) {
  String actualOutput;

  try {
    actualOutput = testEval(input).toString();
  } on StateError catch (se) {
    actualOutput = se.message;
  } on StackOverflowError catch (so) {
    actualOutput = so.toString();
  }

  if (expectedOutput == actualOutput) {
    print("PASS: testEval('$input') => ${expectedOutput} == ${actualOutput}");
  } else {
    print("FAILED: testEval('$input') => ${expectedOutput} != ${actualOutput}");
  }

  if (!isFailing) {
    assert(expectedOutput == actualOutput);
  }
}
