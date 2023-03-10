@x = global double 5.000000e+00
@y = global double 0.000000e+00

define double @f2() {
  %1 = load double, double* @x, align 8
  %2 = fdiv double %1, 2.000000e+00
  ret double %2
}

define void @f3() {
  %1 = load double, double* @x, align 8
  %2 = fmul double %1, 2.000000e+00
  store double %2, double* @y, align 8
  ret void
}

define double @f4() {
  %1 = load double, double* @x, align 8
  %2 = fadd double %1, 1.000000e+00
  %3 = load double, double* @y, align 8
  %4 = fsub double %2, %3
  ret double %4
}

define double @eval() {
  %1 = call double @f2()
  call void @f3()
  %2 = call double @f4()
  ret double %2
}
