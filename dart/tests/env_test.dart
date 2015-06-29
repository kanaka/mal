library env_test;

import "../types.dart";
import "../env.dart";

void main() {
  Env env1 = new Env();
  String key1 = "key1";
  MalSymbol malSymbol1 = new MalSymbol("value1");
  assert(env1.find(new MalSymbol(key1)) == null);
  env1.set(new MalSymbol(key1), malSymbol1);
  assert(env1.find(new MalSymbol(key1)) == malSymbol1);
  assert(env1.get(new MalSymbol(key1)) == malSymbol1);

  Env env2 = new Env(outer: env1);
  String key2 = "key2";
  MalSymbol malSymbol2 = new MalSymbol("value2");
  assert(env2.find(new MalSymbol(key1)) == malSymbol1);
  assert(env2.get(new MalSymbol(key1)) == malSymbol1);
  env1.set(new MalSymbol(key2), malSymbol2);
  assert(env2.find(new MalSymbol(key2)) == malSymbol2);
  assert(env2.get(new MalSymbol(key2)) == malSymbol2);

  var failed = false;
  String key3 = "key3";

  try {
    env2.get(new MalSymbol(key3));
  } on StateError catch (ex) {
    failed = true;
    assert(ex.message == "'$key3' not found");
  }

  assert(failed);
}