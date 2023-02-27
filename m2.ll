@.fstr = private constant [2 x i8] c"%f"

declare i32 @printf(i8*, ...)

; line 1
; x = 5
@x = global double 0.0

define void @l1() {
  %1 = load double, double* @x
  store double 5.0, double* @x
  ret void
}

; line 2
; x / 2
define double @l2() {
  %1 = load double, double* @x
  %2 = fdiv double %1, 2.0
  ret double %2
}

; line 3
; y = x * 2
@y = global double 0.0

define void @l3() {
  %1 = load double, double* @x
  %2 = fmul double %1, 2.0
  store double %2, double* @y
  ret void
}

; line 4
; x + 1 - y
define double @l4() {
  %1 = load double, double* @x
  %2 = fadd double %1, 1.0
  %3 = load double, double* @y
  %4 = fsub double %2, %3
  ret double %4
}

define void @main() {
  call void @l1()
  %1 = call double @l2()
  call void @l3()
  %2 = call double @l4()

  %3 = getelementptr [2 x i8], [2 x i8]* @.fstr, i32 0, i32 0
  %4 = call i32 (i8*, ...) @printf(i8* %3, double %2)
  ret void
}
