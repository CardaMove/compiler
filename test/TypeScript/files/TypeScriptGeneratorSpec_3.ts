export type MyStruct1 = ConStr0<[Integer, MyStruct2]>;

export function MyStruct1(field_0: Integer, field_1: MyStruct2): MyStruct1 {
  return { constructor: 0, fields: [field_0, field_1] };
}
