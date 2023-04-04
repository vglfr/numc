define double @f(double %x) {
  %1 = fadd double %x, %x
  %2 = fmul double %1, %x
  ret double %2
}

define void @eval() {
  ret void
}
