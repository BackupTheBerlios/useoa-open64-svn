!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with ptrs
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine head(x, y) 
  double precision, intent(in) :: x
  double precision, intent(inout) :: y
  double precision, target :: t1, t2
  double precision, pointer :: p1, p2
  
  p1 => t1
  p2 => t1

  t1=x*y
  t2=p1*x+y
  y=p2+t2

end subroutine
