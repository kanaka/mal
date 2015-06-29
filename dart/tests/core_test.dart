library core_test;

import "../types.dart";
import "../core.dart";

class DummyVarargsFunction extends VarargsFunction {
  DummyVarargsFunction(OnCall onCall): super(onCall);
}

MalNumber malNumber1 = new MalNumber(1);
MalNumber malNumber10 = new MalNumber(10);
MalNumber malNumber100 = new MalNumber(100);
MalNumber malNumber1000 = new MalNumber(1000);

void main() {
  testVarargsFunction();
}

void testVarargsFunction() {
  DummyVarargsFunction dummyVarargsFunction = new DummyVarargsFunction((_) => true);
  assert(dummyVarargsFunction());

  var result = Function.apply(sumBinaryOperator, [malNumber1, malNumber10]);
  assert(result is MalNumber);
  assert(result.number == 11);

  result = Function.apply(minusBinaryOperator, [malNumber1, malNumber10]);
  assert(result is MalNumber);
  assert(result.number == -9);

  result = Function.apply(multiplyBinaryOperator, [malNumber1, malNumber10]);
  assert(result is MalNumber);
  assert(result.number == 10);

  result = Function.apply(divideBinaryOperator, [malNumber100, malNumber10]);
  assert(result is MalNumber);
  assert(result.number == 10);
}
