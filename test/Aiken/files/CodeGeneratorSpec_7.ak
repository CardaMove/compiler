fn test_expressions(a: Int, b: Bool) -> (MyRecord, Bool) {
  let c: Int = a + a
  let c: Int = c / 3
  let (my_record, d): (MyRecord, Int) = (MyRecord { a: c, b: False }, 42)
  let MyRecord { a: my_a, b } = my_record

  if c > 10 {
    (my_record, my_record.b)
  } else {
    (my_record, b)
  }
}
