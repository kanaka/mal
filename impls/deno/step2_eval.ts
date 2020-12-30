import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";

const initialEnv: Env.Env = new Map<string, MalType.MalType>([
  [
    "+",
    MalType.mkFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) + MalType.asNumber(b))
    ),
  ],
  [
    "-",
    MalType.mkFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) - MalType.asNumber(b))
    ),
  ],
  [
    "*",
    MalType.mkFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) * MalType.asNumber(b))
    ),
  ],
  [
    "/",
    MalType.mkFunction(([a, b]) =>
      MalType.mkNumber(MalType.asNumber(a) / MalType.asNumber(b))
    ),
  ],
]);

const read = (str: string): MalType.MalType => Reader.readStr(str);

const evaluate_ast = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  if (ast.tag === "MalSymbol") {
    const binding = Env.lookup(ast.name, env);

    if (binding === undefined) {
      throw new Error(`Unknown Symbol: ${ast.name}`);
    } else {
      return binding;
    }
  } else if (ast.tag === "MalList") {
    return MalType.mkList(ast.items.map((i) => evaluate(i, env)));
  } else if (ast.tag === "MalVector") {
    return MalType.mkVector(ast.items.map((i) => evaluate(i, env)));
  } else if (ast.tag === "MalHashMap") {
    return MalType.mkHashMap(
      MalType.mapValues(ast).map(([k, v]) => [k, evaluate(v, env)]),
    );
  } else {
    return ast;
  }
};

const evaluate = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  if (ast.tag === "MalList") {
    if (ast.items.length === 0) {
      return ast;
    } else {
      const evalList = evaluate_ast(ast, env);

      if (evalList.tag === "MalList" || evalList.tag === "MalVector") {
        const [callerItem, ...callerArgs] = evalList.items;

        if (callerItem !== undefined) {
          if (callerItem.tag === "MalFunction") {
            return callerItem.f(callerArgs);
          } else if (callerItem.tag === "MalSymbol") {
            const binding = Env.lookup(callerItem.name, env);

            if (binding !== undefined && binding.tag === "MalFunction") {
              return binding.f(callerArgs);
            }
          }
        }
      }
      throw new Error(`Unable to invoke: ${JSON.stringify(evalList)}`);
    }
  } else {
    return evaluate_ast(ast, env);
  }
};

const print = (exp: MalType.MalType): string => Printer.prStr(exp);

const rep = (str: string): string => print(evaluate(read(str), initialEnv));

type FunctionType = (a: MalType.MalType, b: MalType.MalType) => MalType.MalType;

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
