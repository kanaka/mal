import * as MalType from "./types.ts";

export type Scope = Map<string, MalType.MalType>;

export type Env = {
  outer: Env | undefined;
  data: Scope;
};

export const mkEnv = (outer: Env | undefined = undefined): Env => ({
  outer,
  data: new Map(),
});

export const find = (name: string, env: Env): MalType.MalType | undefined => {
  const result = env.data.get(name);

  if (result === undefined) {
    return env.outer === undefined ? undefined : find(name, env.outer);
  } else {
    return result;
  }
};

export const get = (name: string, env: Env): MalType.MalType => {
  const result = find(name, env);

  if (result === undefined) {
    throw new Error(`Undefined Symbol: ${name} not found`);
  } else {
    return result;
  }
};

export const set = (name: string, value: MalType.MalType, env: Env): void => {
  env.data.set(name, value);
};
