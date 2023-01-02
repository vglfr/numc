define i32 @div1() {
  %1 = call i32 @mul1()
  %2 = sdiv i32 %1, 4
  ret i32 %2
}

define i32 @mul1() {
  %1 = call i32 @add1()
  %2 = mul i32 %1, 2
  ret i32 %2
}

define i32 @add1() {
  %1 = call i32 @sub1()
  %2 = add i32 %1, 6
  ret i32 %2
}

define i32 @sub1() {
  %1 = sub i32 1, 3
  ret i32 %1
}