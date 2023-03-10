@x = global double 0.000000e+00

define void @f1() {
  %1 = fadd double 5.000000e+00, 3.000000e+00
  store double %1, double* @x, align 8
  ret void
}

define void @f2() {
  %1 = load double, double* @x, align 8
  %2 = fmul double %1, 2.000000e+00
  store double %2, double* @x, align 8
  ret void
}

define double @f3() {
  %1 = load double, double* @x, align 8
  %2 = fdiv double %1, 5.000000e+00
  ret double %2
}

define double @eval() {
  call void @f1()
  call void @f2()
  %1 = call double @f3()
  ret double %1
}
