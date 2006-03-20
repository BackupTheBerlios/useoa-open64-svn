!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a subroutine.  Can use this to test interprocedural
! side effect analysis.
! This one uses double precision vars instead of reals.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine foo(y)
double precision, intent(inout) :: y
double precision x

x=y
y=x*2
end


program bar
double precision :: a, b
!real :: g2=1.5
double precision :: g2

!  g = 1.5
a = b+g2
call foo(g2)
end

