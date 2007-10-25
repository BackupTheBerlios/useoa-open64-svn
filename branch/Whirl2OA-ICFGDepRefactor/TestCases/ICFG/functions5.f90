!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a function (instead of subroutine)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function bar(n) result(res)
  integer res, n
  if (n .EQ. 1) then
     res = 1
  else
     res = 5
  end if
end function bar


function foo(n) result(res)
  integer res, n
  if (n .EQ. 1) then
     res = bar(1)
  else
     res = 5
  end if
end function foo

program functiontest

  integer :: n = 7
  real :: x = 4.5

  n = foo(n)
  ! call foo(n) ! what happens to return?

  x = sin(x)
  
end program functiontest


