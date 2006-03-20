!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a subroutine.  Can use this to test interprocedural
! side effect analysis.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine foo(y)
real, intent(inout) :: y
real x

x=y
y=x*2
end


program bar
real :: a, b
!real :: g2=1.5
real :: g2

!  g = 1.5
a = b+g2
call foo(g2)
end

