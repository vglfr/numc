@x = global double 0.000000e+00
@y = global double 0.000000e+00

define void @f1() {
  %1 = fadd double 5.000000e+00, 2.000000e+00
  store double %1, double* @x, align 8
  ret void
}

define double @f2() {
  %1 = fmul double 3.000000e+00, 2.000000e+00
  %2 = fdiv double %1, 4.000000e+00
  %3 = fsub double %2, 6.000000e+00
  ret double %3
}

define void @f3() {
  %1 = fmul double 3.000000e+00, 2.000000e+00
  store double %1, double* @y, align 8
  ret void
}

define double @f4() {
  %1 = fsub double 3.000000e+00, 4.000000e+00
  %2 = fadd double %1, 6.000000e+00
  ret double %2
}

define double @eval() {
  call void @f1()
  %1 = call double @f2()
  call void @f3()
  %2 = call double @f4()
  ret double %2
}
