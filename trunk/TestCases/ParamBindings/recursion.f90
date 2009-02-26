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

    end program recfunc



 
! ParamBindings (Formal => Actual) : 
! ================================== 
! 
! - CallHandle = factorial(n-1)
! 
!     1. (n-1)  => MemRefNode(&n)
! 
!
! - CallHandle = factorial(n)
!
!     1. n      => MemRefNode(&n)
!
