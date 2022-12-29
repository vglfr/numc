@.fstr = private constant [2 x i8] c"%d"

declare i32 @printf(i8*, ...)

define i32 @add(i32 %a, i32 %b) {
  %1 = add i32 %a, %b
  ret i32 %1
}

define void @main() {
  %1 = getelementptr [2 x i8], [2 x i8]* @.fstr, i32 0, i32 0
  %2 = call i32 @add(i32 0, i32 97)

  call i32 (i8*, ...) @printf(i8* %1, i32 %2)
  ret void
}