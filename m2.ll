@.fstr = private constant [2 x i8] c"%f"

declare i32 @printf(i8*, ...)

; line 1
; x = 5
@x = global double 0.0

define void @f1() {
  %1 = load double, double* @x
  store double 5.0, double* @x
  ret void
}

; line 2
; x / 2
define double @f2() {
  %1 = load double, double* @x
  %2 = fdiv double %1, 2.0
  ret double %2
}

; line 3
; y = x * 2
@y = global double 0.0

define void @f3() {
  %1 = load double, double* @x
  %2 = fmul double %1, 2.0
  store double %2, double* @y
  ret void
}

; line 4
; x + 1 - y
define double @f4() {
  %1 = load double, double* @x
  %2 = fadd double %1, 1.0
  %3 = load double, double* @y
  %4 = fsub double %2, %3
  ret double %4
}

; eval
define double @eval() {
  call void @f1()
  %1 = call double @f2()
  call void @f3()
  %2 = call double @f4()
  ret double %2
}

define void @main() {
  %1 = getelementptr [2 x i8], [2 x i8]* @.fstr, i32 0, i32 0
  %2 = call double @eval()
  %3 = call i32 (i8*, ...) @printf(i8* %1, double %2)
  ret void
}
