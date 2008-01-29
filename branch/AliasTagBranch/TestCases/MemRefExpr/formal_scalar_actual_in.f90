!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Scalar Formal Parameters passed as actual argument 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine foo(x,y)
        double precision x
        double precision y
        y=x*2
      end subroutine

      
      subroutine head(x,y)
        double precision, intent(in) :: x, y
        call foo(x,y)
       end subroutine

