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
          a = sin(a+b)/func(f)
        end subroutine foo

        function func(n) result(res)
          double precision res
          double precision n
             res = n
        end function func
