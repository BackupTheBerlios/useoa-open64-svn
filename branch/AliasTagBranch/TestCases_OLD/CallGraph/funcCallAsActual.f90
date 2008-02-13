!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a function calls as actual parameters
! 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function bar(n) result(res)
  integer res, n
  if (n .GT. 1) then
     res = 1
  else
     res = 2*n
  end if
end function bar


function foo(n) result(res)
  integer res, n
  if (n .EQ. 1) then
     res = 2*n
  else
     res = 5
  end if
end function foo

program functiontest

  integer :: n = 7
  real :: x = 4.5

  n = foo(bar(n))
  
end program functiontest


