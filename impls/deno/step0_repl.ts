const read = (str: string): any => str;

const eval_ = (ast: any): any => ast;

const print = (exp: any): string => exp.toString();

const rep = (str: string): string => print(eval_(read(str)));

while (true) {
  const value = prompt("user>");

  if (value === null) {
      break;
  }

  console.log(rep(value));
}
