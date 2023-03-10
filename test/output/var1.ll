@x = global double 5.000000e+00

define double @f1() {
  %1 = load double, double* @x, align 8
  ret double %1
}

define double @eval() {
  %1 = call double @f1()
  ret double %1
}
