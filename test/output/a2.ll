@x = global double 0.000000e+00

define void @f1() {
  %1 = fdiv double 1.000000e+00, 4.000000e+00
  %2 = fadd double 5.000000e+00, %1
  %3 = fmul double 3.000000e+00, 2.000000e+00
  %4 = fsub double %2, %3
  store double %4, double* @x, align 8
  ret void
}

define void @eval() {
  call void @f1()
  ret void
}
