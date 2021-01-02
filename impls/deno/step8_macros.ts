import * as Core from "./core.ts";
import * as Env from "./env.ts";
import * as MalType from "./types.ts";
import * as Printer from "./printer.ts";
import * as Reader from "./reader.ts";

const read = (str: string): MalType.MalType => Reader.readStr(str);

const evaluate_ast = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  switch (ast.tag) {
    case "MalSymbol":
      return Env.get(ast, env);
    case "MalList":
      return MalType.mkList(ast.items.map((i) => evaluate(i, env)));
    case "MalVector":
      return MalType.mkVector(ast.items.map((i) => evaluate(i, env)));
    case "MalHashMap":
      return MalType.mkHashMap(
        MalType.mapValues(ast).map(([k, v]) => [k, evaluate(v, env)]),
      );
    default:
      return ast;
  }
};

const evaluate = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  while (true) {
    ast = macroExpand(ast, env);
    if (ast.tag === "MalList") {
      if (ast.items.length === 0) {
        return ast;
      }

      if (ast.items[0].tag === "MalSymbol") {
        switch (ast.items[0].name) {
          case "def!":
            return evaluateDefBang(ast, env);
          case "defmacro!":
            return evaluateDefMacroBang(ast, env);
          case "do":
            ast = evaluateDo(ast, env);
            continue;
          case "fn*":
            return evaluateFnStar(ast, env);
          case "if":
            ast = evaluateIf(ast, env);
            continue;
          case "let*": {
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

            ast = ast.items[2];
            env = innerEnv;
            continue;
          }
          case "macroexpand":
            return macroExpand(ast.items[1], env);
          case "quasiquote":
            ast = evaluateQuasiQuote(ast.items[1]);
            continue;
          case "quasiquoteexpand":
            return evaluateQuasiQuote(ast.items[1]);
          case "quote":
            return ast.items[1];
        }
      }

      const evalList = evaluate_ast(ast, env);

      if (evalList.tag === "MalList" || evalList.tag === "MalVector") {
        const [callerItem, ...callerArgs] = evalList.items;

        if (callerItem !== undefined) {
          if (callerItem.tag === "MalInternalFunction") {
            return callerItem.fn(callerArgs);
          } else if (callerItem.tag === "MalFunction") {
            ast = callerItem.body;
            env = Env.mkEnv(callerItem.env, callerItem.params, callerArgs);
            continue;
          }
        }
      }

      throw new Error(`Unable to invoke: ${JSON.stringify(evalList)}`);
    } else {
      return evaluate_ast(ast, env);
    }
  }
};

const macroExpand = (ast: MalType.MalType, env: Env.Env): MalType.MalType => {
  const macroFunction = (
    ast: MalType.MalType,
    env: Env.Env,
  ): [MalType.MalFunction, Array<MalType.MalType>] | undefined => {
    if (
      (ast.tag === "MalList" || ast.tag === "MalVector") &&
      ast.items.length > 0 && ast.items[0].tag === "MalSymbol"
    ) {
      const value = Env.find(ast.items[0], env);

      return value !== undefined && value.tag === "MalFunction" && value.isMacro
        ? [value, ast.items.slice(1)]
        : undefined;
    } else {
      return undefined;
    }
  };

  while (true) {
    const macro = macroFunction(ast, env);
    if (macro === undefined) {
      return ast;
    } else {
      ast = evaluate(
        macro[0].body,
        Env.mkEnv(macro[0].env, macro[0].params, macro[1]),
      );
    }
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

const evaluateDefMacroBang = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  if (ast.items[1].tag !== "MalSymbol") {
    throw new Error(
      `Invalid Argument: defmacro! requires a symbol name: ${ast.items[1]}`,
    );
  }

  const result = evaluate(ast.items[2], env);

  if (result.tag === "MalFunction") {
    result.isMacro = true;
  }

  Env.set(ast.items[1], result, env);

  return result;
};

const evaluateDo = (
  ast: MalType.MalList,
  env: Env.Env,
): MalType.MalType => {
  evaluate_ast(MalType.mkList(ast.items.slice(1, ast.items.length - 1)), env);
  return ast.items[ast.items.length - 1];
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

  return MalType.mkFunction(body, formalParameters, env);
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
    return ast.items.length === 4 ? ast.items[3] : MalType.nil;
  } else {
    return ast.items[2];
  }
};

const evaluateQuasiQuote = (ast: MalType.MalType): MalType.MalType => {
  const startsWith = (
    items: Array<MalType.MalType>,
    symbolName: string,
  ): boolean => (items.length == 2 && items[0].tag === "MalSymbol" &&
    items[0].name === symbolName);

  const loop = (
    element: MalType.MalType,
    accumulator: MalType.MalList,
  ): MalType.MalList =>
    (element.tag == "MalList" && startsWith(element.items, "splice-unquote"))
      ? MalType.mkList(
        [MalType.mkSymbol("concat"), element.items[1], accumulator],
      )
      : MalType.mkList(
        [MalType.mkSymbol("cons"), evaluateQuasiQuote(element), accumulator],
      );

  const foldr = (xs: MalType.MalType[]): MalType.MalList => {
    let acc = MalType.mkList([]);
    for (let i = xs.length - 1; i >= 0; i -= 1) {
      acc = loop(xs[i], acc);
    }
    return acc;
  };

  switch (ast.tag) {
    case "MalSymbol":
      return MalType.mkList([MalType.mkSymbol("quote"), ast]);
    case "MalHashMap":
      return MalType.mkList([MalType.mkSymbol("quote"), ast]);
    case "MalList":
      return (startsWith(ast.items, "unquote"))
        ? ast.items[1]
        : foldr(ast.items);
    case "MalVector":
      return MalType.mkList([MalType.mkSymbol("vec"), foldr(ast.items)]);
    default:
      return ast;
  }
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

  Env.set(
    MalType.mkSymbol("eval"),
    MalType.mkInternalFunction(([a]) => {
      if (a === undefined) {
        return MalType.nil;
      }
      return evaluate(a, env);
    }),
    env,
  );

  rep("(def! not (fn* (a) (if a false true)))", env);
  rep(
    '(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))',
    env,
  );
  rep(
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
    env,
  );

  return env;
};

const repl = (env: Env.Env) => {
  while (true) {
    const value = prompt("user>");

    if (value === null) {
      break;
    }

    try {
      console.log(rep(value, env));
    } catch (e) {
      if (e.message !== "Reader Error: No input") {
        console.error(e.message);
      }
    }
  }
};

if (Deno.args.length > 0) {
  const env = initReplEnv();

  Env.set(
    MalType.mkSymbol("*ARGV*"),
    MalType.mkList(Deno.args.slice(1).map(MalType.mkString)),
    env,
  );

  rep(`(load-file "${Deno.args[0]}")`, env);
} else {
  const env = initReplEnv();
  Env.set(MalType.mkSymbol("*ARGV*"), MalType.mkList([]), env);

  repl(env);
}
