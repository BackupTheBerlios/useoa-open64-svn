!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Arrays Local Parameters passed as actual argument
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      subroutine foo(x,y)
        double precision x
        double precision y
        y=x*2
      end subroutine

      
      subroutine head()
        double precision, dimension(2) :: p, q
        integer k,l
        k=1
        p(1)=1.0
        q(1)=2.0
        call foo(p(k),q(k))
       end subroutine

