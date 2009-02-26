!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Scalar Local Parameters passed as actual argument
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine foo(x,y)
        double precision x
        double precision y
        y=x*2
      end subroutine

      
      subroutine head()
        double precision p 
        double precision q
        p=1.0
        q=2.0
        call foo(p,q)
       end subroutine

