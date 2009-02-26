!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Arrays Formal Parameters passed as actual argument 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine foo(x,y)
       double precision, dimension(2) :: x,y
        y(i)=x(i)*2
      end subroutine

      
      subroutine head(x,y)
        double precision, dimension(2), intent(in) :: x, y
        call foo(x,y)
       end subroutine

