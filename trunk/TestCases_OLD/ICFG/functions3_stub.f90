! functions3_stub.f90
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a function (instead of subroutine), using stubs
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function foo(n) result(res)
  integer res, n
  if (n .EQ. 1) then
     res = 1
  else
     res = 5
  end if
end function foo

function sin(n) result(res)
  real res, n
  res = 2.0 * n
end function sin

subroutine end()
  integer n
  n = 5
end subroutine end

program functiontest

  integer :: n = 7
  real :: x = 4.5

  n = foo(n)
  ! call foo(n) ! what happens to return?

  x = sin(x)
  
end program functiontest
