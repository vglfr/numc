define i32 @expr() {
  %1 = sub i32 1, 3
  %2 = add i32 %1, 6

  %3 = mul i32 %2, 2
  %4 = sdiv i32 %3, 4

  ret i32 %4
}
