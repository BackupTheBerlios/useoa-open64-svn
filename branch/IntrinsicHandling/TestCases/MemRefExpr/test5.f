!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Arrays Local Parameters passed as actual argument
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      subroutine foo(x,y)
        double precision, dimension(2) :: x
        double precision y
        y=x(1)*x(2)
      end subroutine

      
      subroutine head()
        double precision, dimension(2) :: x, y 
        call foo(x,y)
       end subroutine

