import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";
import { readline } from "./readline.ts";


const replEnv = Env.mkEnv();

Env.set(
  MalType.mkSymbol("+"),
  MalType.mkInternalFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) + MalType.asNumber(b))
  ),
  replEnv,
);

Env.set(
  MalType.mkSymbol("-"),
  MalType.mkInternalFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) - MalType.asNumber(b))
  ),
  replEnv,
);

Env.set(
  MalType.mkSymbol("*"),
  MalType.mkInternalFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) * MalType.asNumber(b))
  ),
  replEnv,
);

Env.set(
  MalType.mkSymbol("/"),
  MalType.mkInternalFunction(([a, b]) =>
    MalType.mkNumber(MalType.asNumber(a) / MalType.asNumber(b))
  ),
  replEnv,
);

const read = (str: string): MalType.MalType => Reader.readStr(str);

const evaluate_ast = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  if (ast.tag === "MalSymbol") {
    return Env.get(ast, env);
  } else if (ast.tag === "MalList") {
    return MalType.mkList(ast.items.map((i) => evaluate(i, env)));
  } else if (ast.tag === "MalVector") {
    return MalType.mkVector(ast.items.map((i) => evaluate(i, env)));
  } else if (ast.tag === "MalHashMap") {
    return MalType.mkHashMap(
      MalType.mapKeyValues(ast).map(([k, v]) => [k, evaluate(v, env)]),
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
      return evaluateDefBang(ast, env);
    } else if (isNamedSymbol(ast.items[0], "let*")) {
      return evaluateLetStar(ast, env);
    } else {
      return evaluateFunctionInvocation(ast, env);
    }
  } else {
    return evaluate_ast(ast, env);
  }
};

const evaluateDefBang = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  if (ast.items[1].tag !== "MalSymbol") {
    throw new Error(
      `Invalid Argument: def! requires a symbol name: ${ast.items[1]}`,
    );
  }

  const result = evaluate(ast.items[2], env);
  Env.set(ast.items[1], result, env);

  return result;
};

const evaluateLetStar = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  const bindings = ast.items[1];

  if (bindings.tag !== "MalList" && bindings.tag !== "MalVector") {
    throw new Error(
      `Invalid Argument: let* requires a list of bindings: ${
        JSON.stringify(bindings)
      }`,
    );
  }

  const innerEnv = Env.mkEnv(env);
  for (let lp = 0; lp < bindings.items.length; lp += 2) {
    const name = bindings.items[lp];
    const value = bindings.items[lp + 1] ?? MalType.nil;

    if (name.tag !== "MalSymbol") {
      throw new Error(
        `Invalid Argument: let* binding requires a symbol name: ${
          JSON.stringify(name)
        }`,
      );
    }

    Env.set(name, evaluate(value, innerEnv), innerEnv);
  }

  return evaluate(ast.items[2], innerEnv);
};

const evaluateFunctionInvocation = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  const evalList = evaluate_ast(ast, env);

  if (evalList.tag === "MalList" || evalList.tag === "MalVector") {
    const [callerItem, ...callerArgs] = evalList.items;

    if (callerItem !== undefined) {
      if (callerItem.tag === "MalInternalFunction") {
        return callerItem.fn(callerArgs);
      } else if (callerItem.tag === "MalFunction") {
        return evaluate(
          callerItem.body,
          Env.mkEnv(callerItem.env, callerItem.params, callerArgs),
        );
      }
    }
  }

  throw new Error(`Unable to invoke: ${JSON.stringify(evalList)}`);
};

const print = (exp: MalType.MalType): string => Printer.prStr(exp);

const rep = (str: string): string => print(evaluate(read(str), replEnv));

while (true) {
  const value = readline("user> ");

  if (value === undefined) {
    break;
  } else if (value === "") {
    continue;
  }

  try {
    console.log(rep(value));
  } catch (e) {
    console.error(e.message);
  }
}
