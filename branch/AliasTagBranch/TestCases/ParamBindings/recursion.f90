!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with a recursive function
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function factorial(n) result(res)
  integer res, n
  if (n .EQ. 1) then
     res = 1
  else
     res = n*factorial(n-1)

!                            Formal             => Actual
!                            factorial::*(n-1)  => MemRefNode(factorial::n)


  end if
end function factorial


program recfunc
interface 
recursive function factorial(n) result(res)
  integer res, n
end function
end interface
  integer :: n = 4

  n = factorial(n)

!                   Formal         => Actual
!                   factorial::*n  => MemRefNode(factorial::n)

end program recfunc
