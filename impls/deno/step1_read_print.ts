import * as Reader from "./reader.ts";
import * as Printer from "./printer.ts";
import { MalType } from "./types.ts";

const read = (str: string): MalType => Reader.readStr(str);

const eval_ = (ast: MalType): MalType => ast;

const print = (exp: MalType): string => Printer.prStr(exp);

const rep = (str: string): string => print(eval_(read(str)));

while (true) {
  const value = prompt("user>");

  if (value === null) {
    break;
  }

  try {
    console.log(rep(value));
  } catch (e) {
    console.error(e.message);
  }
}
