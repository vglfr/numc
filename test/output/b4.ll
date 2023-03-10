define double @f1() {
  %1 = fdiv double 1.000000e+00, 2.000000e+00
  ret double %1
}

define double @eval() {
  %1 = call double @f1()
  ret double %1
}
