!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a recursive function
! Local Variable passed as actual parameter in the expression
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function factorial(n) result(res)
  integer res, n, temp
  if (n .EQ. 1) then
     res = 1
  else
     temp=n-1
     res = n*factorial(temp)
  end if
end function factorial


program recfunc

  integer :: n = 7

  n = factorial(n-1)
  
end program recfunc
