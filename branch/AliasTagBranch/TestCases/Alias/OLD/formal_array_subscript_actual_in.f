!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Arrays Formal Parameters passed as actual argument 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine foo(x,y)
        double precision x
        double precision y
        y=x*2
      end subroutine

      
      subroutine head(x,y)
        double precision, dimension(2), intent(in) :: x,y
        integer k,l
        k=1
        call foo(x(k),y(k))
       end subroutine

