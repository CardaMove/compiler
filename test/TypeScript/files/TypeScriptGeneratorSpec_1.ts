export type MyStruct1 = ConStr0<[Integer, MyStruct2]>;

export function MyStruct1(field1: Integer, field2: MyStruct2): MyStruct1 {
  return { constructor: 0, fields: [field1, field2] };
}
