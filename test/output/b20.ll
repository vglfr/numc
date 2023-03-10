define double @f1() {
  %1 = fadd double 5.000000e+00, 1.000000e+00
  %2 = fsub double %1, 3.000000e+00
  %3 = fmul double 6.000000e+00, 2.000000e+00
  %4 = fdiv double %3, 4.000000e+00
  %5 = fadd double %2, %4
  ret double %5
}

define double @eval() {
  %1 = call double @f1()
  ret double %1
}
