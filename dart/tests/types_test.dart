library types_test;

import "../types.dart";

class DummyMalType extends MalType {}

MalNumber malNumber1 = new MalNumber(1);
MalNumber malNumber10 = new MalNumber(10);
MalNumber malNumber100 = new MalNumber(100);
MalNumber malNumber1000 = new MalNumber(1000);

void main() {
  testMalType();
  testMalList();
  testMalVector();
  testMalHashMap();
  testMalHashMapKeys();
  testMalNumber();
  testMalSymbol();
}

void testMalType() {
  var failed = false;

  try {
    MalType malType = new DummyMalType();
    malType();
  } on Error catch(ex) {
    failed = true;
  }

  assert(failed);
}

void testMalList() {
  MalList malList = new MalList();
  malList.malTypes.add(malNumber10);
  assert(malList.toString() == "(10)");
}

void testMalVector() {
  MalVector malVector = new MalVector();
  malVector.malTypes.add(malNumber10);
  assert(malVector.toString() == "[10]");
}

void testMalHashMap() {
  MalHashMap malHashMap = new MalHashMap();
  malHashMap.malHashMap[new MalKeyword(":sym")] = malNumber10;
  assert(malHashMap.toString() == "{:sym ${malNumber10.toString()}}");
}

void testMalHashMapKeys() {
  MalHashMap malHashMap = new MalHashMap();
  malHashMap.malHashMap[new MalKeyword(":sym")] = malNumber10;
  assert(malHashMap.toString() == "{:sym ${malNumber10.toString()}}");

  malHashMap.malHashMap[new MalKeyword(":sym")] = malNumber1000;
  assert(malHashMap.toString() == "{:sym ${malNumber1000.toString()}}");
}

void testMalNumber() {
  int n = 10;
  MalNumber malNumber = new MalNumber(n);
  assert(malNumber.number == n);
  assert(malNumber.toString() == n.toString());
}

void testMalSymbol() {
  String sym = "sym";
  MalSymbol malSymbol = new MalSymbol(sym);
  assert(malSymbol.symbol == sym);
  assert(malSymbol.toString() == sym);
}
