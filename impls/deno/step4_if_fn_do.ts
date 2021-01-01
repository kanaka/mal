import * as Core from "./core.ts";
import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";

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
      return evaluateDefBang(ast, env);
    } else if (isNamedSymbol(ast.items[0], "do")) {
      return evaluateDo(ast, env);
    } else if (isNamedSymbol(ast.items[0], "fn*")) {
      return evaluateFnStar(ast, env);
    } else if (isNamedSymbol(ast.items[0], "if")) {
      return evaluateIf(ast, env);
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

const evaluateDo = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  const result = evaluate_ast(MalType.mkList(ast.items.slice(1)), env);

  if (result.tag !== "MalList") {
    throw new Error(
      `Invalid Argument: do expected a list: ${JSON.stringify(result)}`,
    );
  }
  if (result.items.length === 0) {
    throw new Error("Invalid Argument: do expected a non-empty list");
  }

  return result.items[result.items.length - 1];
};

const evaluateFnStar = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  const [_, parameters, body] = ast.items;

  if (
    parameters == undefined ||
    parameters.tag !== "MalList" && parameters.tag !== "MalVector"
  ) {
    throw new Error("Invalid Argument: fn* expects a list of parameters");
  }
  if (body === undefined) {
    throw new Error("Invalid Argument: fn* expects a body");
  }

  const formalParameters: Array<MalType.MalSymbol> = [];
  parameters.items.forEach((p, idx) => {
    if (p.tag === "MalSymbol") {
      formalParameters.push(p);
    } else {
      throw new Error(
        `Invalid Argument: Parameter ${idx + 1} in fn* is not a symbol: ${
          JSON.stringify(p)
        }`,
      );
    }
  });

  const fn = (args: Array<MalType.MalType>): MalType.MalType =>
    evaluate(body, Env.mkEnv(env, formalParameters, args));

  return MalType.mkInternalFunction(fn);
};

const evaluateIf = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  if (ast.items.length < 3) {
    throw new Error(
      `Invalid Argument: if expects at least 3 arguments: ${
        JSON.stringify(ast)
      }`,
    );
  }
  if (ast.items.length > 4) {
    throw new Error(
      `Invalid Argument: if expects no more than 4 arguments: ${
        JSON.stringify(ast)
      }`,
    );
  }

  const ifGuard = evaluate(ast.items[1], env);

  if (
    ifGuard.tag === "MalNil" || ifGuard.tag === "MalBoolean" && !ifGuard.value
  ) {
    return ast.items.length === 4 ? evaluate(ast.items[3], env) : MalType.nil;
  } else {
    return evaluate(ast.items[2], env);
  }
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

const rep = (str: string, env: Env.Env): string =>
  print(evaluate(read(str), env));

const initReplEnv = () => {
  const ns = Core.ns(evaluate);

  const env = Env.mkEnv(
    undefined,
    ns.map(([n]) => MalType.mkSymbol(n)),
    ns.map(([, t]) => t),
  );

  rep("(def! not (fn* (a) (if a false true)))", env);

  return env;
};

const repl = () => {
  const env = initReplEnv();

  while (true) {
    const value = prompt("user>");

    if (value === null) {
      break;
    }

    try {
      console.log(rep(value, env));
    } catch (e) {
      console.error(e.message);
    }
  }
};

repl();
