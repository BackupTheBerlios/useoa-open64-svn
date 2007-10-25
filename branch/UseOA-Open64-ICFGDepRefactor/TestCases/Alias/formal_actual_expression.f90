!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a recursive function
! Formal Parameter pass as actual parameter in the Expression
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function factorial(n) result(res)
  integer res, n
  if (n .EQ. 1) then
     res = 1
  else
     res = n*factorial(n-1)
  end if
end function factorial


program recfunc

  integer :: n = 7

  n = factorial(n)
  
end program recfunc
