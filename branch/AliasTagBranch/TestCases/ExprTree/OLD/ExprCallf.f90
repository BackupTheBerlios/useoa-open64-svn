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
          a = a+func(f*x,t)
        end subroutine foo

        function func(n,m) result(res)
          double precision res
          double precision, intent(in) :: n
          double precision m
             res = n
        end function func

