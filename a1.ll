@.fstr = private constant [2 x i8] c"%f"

declare i32 @printf(i8*, ...)

; define double @eval() {
;   ret double 5.000000e+00
; }

@x = global double 5.0

define void @main() {
  %1 = getelementptr [2 x i8], [2 x i8]* @.fstr, i32 0, i32 0
  %2 = load double, double* @x
  %3 = call i32 (i8*, ...) @printf(i8* %1, double %2)
  ret void
}
