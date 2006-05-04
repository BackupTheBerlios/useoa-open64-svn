!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with X as the independent variable and F as the dependent
! t3 and the third statement are not active.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine head(x, f) 
  double precision, intent(in) :: x
  double precision, intent(inout) :: f
  double precision t1, t2, t3

  t1=x*f
  t2=t1*x+f
  t3=f*30
  f=t1+t2

end subroutine
