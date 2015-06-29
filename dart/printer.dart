library mal.printer;

import "dart:io";
import "types.dart";

String pr_str(MalType malType, [bool printReadable = false]) {
  return malType.toString(printReadable);
}

//String pr_str_args(MalType malType, String sep, bool printReadable) {
//
//}