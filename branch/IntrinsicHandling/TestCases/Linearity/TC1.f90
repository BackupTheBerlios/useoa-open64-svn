! LinearityAnalysis for Automatic Differentiation Example
        program main
          double precision :: a, f, t;
          double precision :: x;
        
          call foo (a,f,x,t);
        end program main

        subroutine foo(a,f,x,t)
          double precision :: a, f, t
          double precision :: x
          integer :: i
          a = 0.0
          f = 0.0
          do i=0,10
            a = a+x*x
            t = SIN(a)
            f = f+t
          end do
        end subroutine foo

