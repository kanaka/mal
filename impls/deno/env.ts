import * as MalType from "./types.ts";

export type Scope = Map<string, MalType.MalType>;

export type Env = {
  outer: Env | undefined;
  data: Scope;
};

export const mkEnv = (
  outer: Env | undefined = undefined,
  binds: Array<MalType.MalSymbol> = [],
  exprs: Array<MalType.MalType> = [],
): Env => {
  const data: Array<[string, MalType.MalType]> = [];

  for (let lp = 0; lp < binds.length; lp += 1) {
    if (binds[lp].name === "&") {
      if (lp !== binds.length - 2) {
        throw new Error(
          `Illegal Argument: the '&' symbol must be the second last parameter`,
        );
      } else {
        data.push([binds[lp + 1].name, MalType.mkList(exprs.slice(lp))]);
        break;
      }
    } else {
      data.push([binds[lp].name, exprs[lp]]);
    }
  }

  return { outer, data: new Map(data) };
};

export const find = (
  name: MalType.MalSymbol,
  env: Env,
): MalType.MalType | undefined => {
  const result = env.data.get(name.name);

  if (result === undefined) {
    return env.outer === undefined ? undefined : find(name, env.outer);
  } else {
    return result;
  }
};

export const get = (name: MalType.MalSymbol, env: Env): MalType.MalType => {
  const result = find(name, env);

  if (result === undefined) {
    throw new Error(`Undefined Symbol: ${name.name} not found`);
  } else {
    return result;
  }
};

export const set = (
  name: MalType.MalSymbol,
  value: MalType.MalType,
  env: Env,
): void => {
  env.data.set(name.name, value);
};
