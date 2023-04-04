define double @f1() {
  ret double 5.500000e+00
}

define double @eval() {
  %1 = call double @f1()
  ret double %1
}
