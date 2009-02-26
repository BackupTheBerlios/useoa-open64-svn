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
          a = ((a*x)+sin(t))
        end subroutine foo

