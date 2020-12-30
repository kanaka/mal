import * as MalType from "./types.ts";

export type Env = Map<string, MalType.MalType>;

export const lookup = (name: string, env: Env): MalType.MalType | undefined =>
  env.get(name);
