/**
 * Plutus `Data` primitives shared by the whole TS port.
 *
 * Values are Mesh JSON Plutus data (`conStr*`, `integer`, `byteString`, `list`, `{ map }`).
 * Mesh has no runtime type checks, structural equality or error type matching Aiken,
 * so those live here.
 */
import {
  ByteString,
  ConStr,
  Integer,
  PlutusData,
  byteString,
  deserializeDatum,
  hexToString,
  integer,
  serializeData,
  stringToHex,
} from "@meshsdk/core";

/** Aiken's untyped `Data`. Mesh's `PlutusData` only admits constructor index 0 (`ConStr` defaults `N` to 0), so any index is added */
export type Data = PlutusData | ConStr<number, any>;

/** Aiken's `Void`, encoded as `Constr 0 []` */
export type Void = ConStr<0, []>;
export const VOID: Void = { constructor: 0, fields: [] };

/** Thrown for every Aiken `fail`, failed `expect` or failing builtin */
export class AikenError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "AikenError";
  }
}

/** Aiken `fail @"message"` */
export function fail(message: string): never {
  throw new AikenError(message);
}

/** A failed Aiken `expect`. The message is the pattern from the Aiken source */
export function expect_fail(pattern: string): never {
  throw new AikenError(`expect ${pattern}`);
}

// ---------------------------------------------------------------------------
// Tracing (Aiken `trace`), off by default
// ---------------------------------------------------------------------------

let traceLogger: ((message: string) => void) | undefined;

/** Enables (or disables, with `undefined`) the output of Aiken `trace`s */
export function setTraceLogger(logger: ((message: string) => void) | undefined): void {
  traceLogger = logger;
}

export function trace(message: string, value?: Data): void {
  if (traceLogger) {
    traceLogger(value === undefined ? message : `${message}: ${toCbor(value)}`);
  }
}

// ---------------------------------------------------------------------------
// Scalars
// ---------------------------------------------------------------------------

/** Builds an `Int` */
export function int(n: number | bigint): Integer {
  return integer(BigInt(n));
}

/** Reads an `Int`. Mesh types `Integer.int` as `number | bigint`, so it is always normalized here */
export function toBigInt(i: Integer): bigint {
  return BigInt(i.int);
}

/** Builds a `ByteArray` from a UTF-8 string, exactly like an Aiken string literal `"addr1"` */
export function str(s: string): ByteString {
  return byteString(stringToHex(s));
}

/** Builds a `ByteArray` from hex, like an Aiken `#"..."` literal. Hex is kept lowercase so that comparisons work */
export function bytes(hex: string): ByteString {
  return byteString(hex.toLowerCase());
}

/** Aiken `string.from_bytearray` (UTF-8 decoding) */
export function from_bytearray(b: ByteString): string {
  return hexToString(b.bytes);
}

// ---------------------------------------------------------------------------
// Runtime shape checks. TS `x is T` exists only at compile time, so every Aiken
// cast (`expect x: T = data`, `if x is T`) goes through one of these
// ---------------------------------------------------------------------------

const hasOwn = (o: object, key: string) => Object.prototype.hasOwnProperty.call(o, key);

export function isConstr(d: unknown): d is ConStr<number, Data[]> {
  // `constructor` must be an own property: every JS object inherits one from Object.prototype
  return (
    typeof d === "object" &&
    d !== null &&
    hasOwn(d, "constructor") &&
    (typeof (d as ConStr).constructor === "number" || typeof (d as ConStr).constructor === "bigint") &&
    Array.isArray((d as ConStr).fields)
  );
}

export function isInt(d: unknown): d is Integer {
  return (
    typeof d === "object" &&
    d !== null &&
    hasOwn(d, "int") &&
    (typeof (d as Integer).int === "number" || typeof (d as Integer).int === "bigint")
  );
}

export function isBytes(d: unknown): d is ByteString {
  return typeof d === "object" && d !== null && hasOwn(d, "bytes") && typeof (d as ByteString).bytes === "string";
}

export function isList(d: unknown): d is { list: Data[] } {
  return typeof d === "object" && d !== null && hasOwn(d, "list") && Array.isArray((d as { list: unknown }).list);
}

export function isMap(d: unknown): d is { map: { k: Data; v: Data }[] } {
  return typeof d === "object" && d !== null && hasOwn(d, "map") && Array.isArray((d as { map: unknown }).map);
}

/** Constructor index of a `Constr`, as a number */
export function constrIndex(d: ConStr<unknown, unknown>): number {
  return Number(d.constructor);
}

/** `builtin.unconstr_fields`: fails if `d` is not a `Constr` */
export function unconstr_fields(d: Data): Data[] {
  if (!isConstr(d)) {
    fail("unconstr_fields: data is not a Constr");
  }
  return d.fields;
}

// ---------------------------------------------------------------------------
// Equality and (de)serialization
// ---------------------------------------------------------------------------

/**
 * Aiken `==` on Data: structural, and order-sensitive for lists and maps.
 * Integers and constructor indices compare by value, whether they are `number` or `bigint`
 */
export function data_equals(a: Data, b: Data): boolean {
  if (isConstr(a)) {
    return (
      isConstr(b) &&
      BigInt(a.constructor) === BigInt(b.constructor) &&
      a.fields.length === b.fields.length &&
      a.fields.every((f, i) => data_equals(f, b.fields[i]))
    );
  }
  if (isInt(a)) {
    return isInt(b) && BigInt(a.int) === BigInt(b.int);
  }
  if (isBytes(a)) {
    return isBytes(b) && a.bytes.toLowerCase() === b.bytes.toLowerCase();
  }
  if (isList(a)) {
    return isList(b) && a.list.length === b.list.length && a.list.every((x, i) => data_equals(x, b.list[i]));
  }
  if (isMap(a)) {
    return (
      isMap(b) &&
      a.map.length === b.map.length &&
      a.map.every((e, i) => data_equals(e.k, b.map[i].k) && data_equals(e.v, b.map[i].v))
    );
  }
  return fail("data_equals: value is not Plutus data");
}

/**
 * Brings decoded data to the canonical form used by the library:
 * constructor indices as `number`, integers as `bigint`, lowercase hex.
 * `deserializeDatum` returns constructor indices as `bigint`, which would break `constructor === 0` checks
 */
export function normalize_data(d: unknown): Data {
  if (isConstr(d)) {
    return { constructor: Number(d.constructor), fields: d.fields.map(normalize_data) };
  }
  if (isInt(d)) {
    return { int: BigInt(d.int) };
  }
  if (isBytes(d)) {
    return { bytes: d.bytes.toLowerCase() };
  }
  if (isList(d)) {
    return { list: d.list.map(normalize_data) };
  }
  if (isMap(d)) {
    return { map: d.map.map(({ k, v }) => ({ k: normalize_data(k), v: normalize_data(v) })) };
  }
  return fail("normalize_data: value is not Plutus data");
}

/** CBOR hex of some data, same bytes as Aiken `builtin.serialise_data` */
export function toCbor(d: Data): string {
  return serializeData(d, "JSON");
}

/** Decodes CBOR hex into normalized data */
export function fromCbor(cbor: string): Data {
  return normalize_data(deserializeDatum(cbor));
}
