define double @f1() {
  %1 = fsub double 4.000000e+00, 3.000000e+00
  ret double %1
}

define double @f2() {
  %1 = fadd double 1.000000e+00, 2.000000e+00
  ret double %1
}

define double @eval() {
  %1 = call double @f1()
  %2 = call double @f2()
  ret double %2
}
