/**
 * Port of the parts of `aiken/primitive/bytearray` used by the library.
 * A `ByteArray` is a Mesh `ByteString` (lowercase hex); indices and lengths count bytes, not hex chars
 */
import { ByteString, byteString } from "@meshsdk/core";
import { fail } from "./data";

export function length(self: ByteString): number {
  return self.bytes.length / 2;
}

/** `builtin.slice_bytearray(start, n, self)`: negative values are clamped to 0, out-of-range to the length */
export function slice_bytearray(start: number, n: number, self: ByteString): ByteString {
  const len = length(self);
  const from = Math.min(Math.max(start, 0), len);
  const to = Math.min(from + Math.max(n, 0), len);
  return byteString(self.bytes.slice(from * 2, to * 2));
}

export function take(self: ByteString, n: number): ByteString {
  return slice_bytearray(0, n, self);
}

export function drop(self: ByteString, n: number): ByteString {
  return slice_bytearray(n, length(self) - n, self);
}

/** Both `start` and `end` are inclusive */
export function slice(self: ByteString, start: number, end: number): ByteString {
  return slice_bytearray(start, end - start + 1, self);
}

/** `builtin.index_bytearray`: fails when out of bounds */
export function at(self: ByteString, index: number): number {
  if (index < 0 || index >= length(self)) {
    fail("index_bytearray: index out of bounds");
  }
  return parseInt(self.bytes.slice(index * 2, index * 2 + 2), 16);
}

/** Prepends a byte */
export function push(self: ByteString, byte: number): ByteString {
  if (byte < 0 || byte > 255) {
    fail("cons_bytearray: byte out of range");
  }
  return byteString(byte.toString(16).padStart(2, "0") + self.bytes);
}

export function concat(left: ByteString, right: ByteString): ByteString {
  return byteString(left.bytes + right.bytes);
}

/** Returns the byte indices `(start, end)` (both inclusive) of the first occurrence of `bytes` */
export function index_of(self: ByteString, bytes: ByteString): [number, number] | undefined {
  const offset = length(bytes);
  for (let cursor = 0; cursor + offset <= length(self); cursor++) {
    if (self.bytes.slice(cursor * 2, (cursor + offset) * 2) === bytes.bytes) {
      return [cursor, cursor + offset - 1];
    }
  }
  return undefined;
}

export function starts_with(self: ByteString, prefix: ByteString): boolean {
  const prefixLength = length(prefix);
  if (length(self) < prefixLength) {
    return false;
  }
  return take(self, prefixLength).bytes === prefix.bytes;
}

/** Folds over the bytes from the last one to the first one */
export function foldr<R>(self: ByteString, zero: R, fn: (byte: number, acc: R) => R): R {
  let acc = zero;
  for (let i = length(self) - 1; i >= 0; i--) {
    acc = fn(at(self, i), acc);
  }
  return acc;
}

/** `builtin.less_than_bytearray`: lexicographic byte order, shorter prefix first */
export function compare(a: ByteString, b: ByteString): number {
  // On lowercase hex, string order is byte order
  const x = a.bytes.toLowerCase();
  const y = b.bytes.toLowerCase();
  return x < y ? -1 : x > y ? 1 : 0;
}
