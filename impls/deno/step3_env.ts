import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";

const replEnv = Env.mkEnv();

Env.set(
  "+",
  MalType.mkFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) + MalType.asNumber(b))
  ),
  replEnv,
);

Env.set(
  "-",
  MalType.mkFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) - MalType.asNumber(b))
  ),
  replEnv,
);

Env.set(
  "*",
  MalType.mkFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) * MalType.asNumber(b))
  ),
  replEnv,
);

Env.set(
  "/",
  MalType.mkFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) / MalType.asNumber(b))
  ),
  replEnv,
);

const read = (str: string): MalType.MalType => Reader.readStr(str);

const evaluate_ast = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  if (ast.tag === "MalSymbol") {
    return Env.get(ast.name, env);
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

const isNamedSymbol = (ast: MalType.MalType, name: string): boolean =>
  ast.tag === "MalSymbol" && ast.name === name;

const evaluate = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  if (ast.tag === "MalList") {
    if (ast.items.length === 0) {
      return ast;
    } else if (isNamedSymbol(ast.items[0], "def!")) {
      if (ast.items[1].tag === "MalSymbol") {
        const result = evaluate(ast.items[2], env);
        Env.set(ast.items[1].name, result, env);
        return result;
      } else {
        throw new Error(
          `Invalid Argument: def! requires a symbol name: ${ast.items[1]}`,
        );
      }
    } else if (isNamedSymbol(ast.items[0], "let*")) {
      const innerEnv = Env.mkEnv(env);

      const bindings = ast.items[1];
      if (bindings.tag === "MalList" || bindings.tag === "MalVector") {
        for (let lp = 0; lp < bindings.items.length; lp += 2) {
          const name = bindings.items[lp];
          const value = bindings.items[lp + 1] ?? MalType.nil;

          if (name.tag === "MalSymbol") {
            Env.set(name.name, evaluate(value, innerEnv), innerEnv);
          } else {
            throw new Error(
              `Invalid Argument: let* binding requires a symbol name: ${
                JSON.stringify(name)
              }`,
            );
          }
        }

        return evaluate(ast.items[2], innerEnv);
      } else {
        throw new Error(
          `Invalid Argument: let* requires a list of bindings: ${
            JSON.stringify(bindings)
          }`,
        );
      }
    } else {
      const evalList = evaluate_ast(ast, env);

      if (evalList.tag === "MalList" || evalList.tag === "MalVector") {
        const [callerItem, ...callerArgs] = evalList.items;

        if (callerItem !== undefined) {
          if (callerItem.tag === "MalFunction") {
            return callerItem.f(callerArgs);
          } else if (callerItem.tag === "MalSymbol") {
            const binding = Env.get(callerItem.name, env);

            if (binding.tag === "MalFunction") {
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

const rep = (str: string): string => print(evaluate(read(str), replEnv));

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
