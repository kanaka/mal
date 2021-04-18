import { readline } from "./readline.ts";

const read = (str: string): any => str;

const eval_ = (ast: any): any => ast;

const print = (exp: any): string => exp.toString();

const rep = (str: string): string => print(eval_(read(str)));

while (true) {
  const value = readline("user> ");

  if (value === undefined) {
      break;
  }

  console.log(rep(value));
}
