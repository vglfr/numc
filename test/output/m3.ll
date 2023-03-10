@x = global double 5.000000e+00
@y = global double 3.000000e+00

define double @f2() {
  %1 = fmul double 3.000000e+00, 2.000000e+00
  %2 = fdiv double %1, 4.000000e+00
  %3 = fsub double %2, 6.000000e+00
  ret double %3
}

define double @f4() {
  %1 = fsub double 3.000000e+00, 4.000000e+00
  %2 = fadd double %1, 6.000000e+00
  ret double %2
}

define double @eval() {
  %1 = call double @f2()
  %2 = call double @f4()
  ret double %2
}
