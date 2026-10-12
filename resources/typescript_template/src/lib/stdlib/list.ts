/**
 * Port of the parts of `aiken/collection/list` whose semantics differ from plain JS arrays
 */

/** `list.at`: `None` (here `undefined`) for negative or out-of-range indices, never an error */
export function at<T>(self: readonly T[], index: bigint | number): T | undefined {
  const i = BigInt(index);
  if (i < 0n || i >= BigInt(self.length)) {
    return undefined;
  }
  return self[Number(i)];
}
