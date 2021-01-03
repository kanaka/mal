export const readline = (
  pp: string | undefined = undefined,
): string | undefined => {
  const result: Array<string> = [];

  if (pp !== undefined) {
    Deno.stdout.writeSync(new TextEncoder().encode(pp));
  }

  while (true) {
    const buffer = new Uint8Array(16);
    const uint8Read = Deno.stdin.readSync(buffer);

    if (uint8Read == null) {
      return undefined;
    } else {
      const decoder = new TextDecoder();
      const text = decoder.decode(buffer).substr(0, uint8Read);

      if (text.endsWith("\n")) {
        result.push(text.substr(0, text.length - 1));
        return result.join("");
      } else {
        result.push(text);
      }
    }
  }
};
