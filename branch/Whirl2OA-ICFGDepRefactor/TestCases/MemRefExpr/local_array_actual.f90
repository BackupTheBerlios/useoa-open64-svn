!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Arrays Formal Parameters passed as actual argument 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine foo(x,y)
       double precision, dimension(2) :: x,y
        y(i)=x(i)*2
      end subroutine

      
      subroutine head()
        double precision, dimension(2) :: p,q
        call foo(p,q)
       end subroutine

