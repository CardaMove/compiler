/**
 * Port of the parts of `aiken/interval` used by the library, as Plutus data:
 * `Interval { lower_bound, upper_bound }`, `IntervalBound { bound_type, is_inclusive }`,
 * `IntervalBoundType = NegativeInfinity | Finite(Int) | PositiveInfinity`
 */
import { Bool, ConStr0, ConStr1, ConStr2, Integer, bool } from "@meshsdk/core";
import { int } from "./data";

export type NegativeInfinity = ConStr0<[]>;
export type Finite = ConStr1<[Integer]>;
export type PositiveInfinity = ConStr2<[]>;
export type IntervalBoundType = NegativeInfinity | Finite | PositiveInfinity;
export type IntervalBound = ConStr0<[IntervalBoundType, Bool]>;
export type Interval = ConStr0<[IntervalBound, IntervalBound]>;

export const negative_infinity = (): NegativeInfinity => ({ constructor: 0, fields: [] });
export const finite = (n: Integer): Finite => ({ constructor: 1, fields: [n] });
export const positive_infinity = (): PositiveInfinity => ({ constructor: 2, fields: [] });

export function interval_bound(bound_type: IntervalBoundType, is_inclusive: boolean): IntervalBound {
  return { constructor: 0, fields: [bound_type, bool(is_inclusive)] };
}

export function interval(lower_bound: IntervalBound, upper_bound: IntervalBound): Interval {
  return { constructor: 0, fields: [lower_bound, upper_bound] };
}

/** `interval.between(lower, upper)`: both bounds inclusive */
export function between(lower: number | bigint, upper: number | bigint): Interval {
  return interval(interval_bound(finite(int(lower)), true), interval_bound(finite(int(upper)), true));
}

/** `interval.everything` */
export function everything(): Interval {
  return interval(interval_bound(negative_infinity(), true), interval_bound(positive_infinity(), true));
}
