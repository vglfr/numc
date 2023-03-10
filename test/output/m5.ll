@x = global double 5.000000e+00

define double @f2() {
  %1 = load double, double* @x, align 8
  %2 = fmul double %1, 2.000000e+00
  ret double %2
}

define double @eval() {
  %1 = call double @f2()
  ret double %1
}
