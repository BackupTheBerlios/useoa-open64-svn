       subroutine head()
         double precision x(2)
         double precision y(2)
         integer i
c$openad INDEPENDENT(x)
         i = 1
         x(1) = 5
         x(1) = 6 + x(1)
         x(2) = 7 + x(1)
         y(1) = x(1)
         y(2) = x(i)
         y(i) = x(2)
         y(1) = x(i+1)
         y(i) = y(i) + x(i)

         y(2) = foo()
c$openad DEPENDENT(y)
       end subroutine
       
       function foo() result (res)
       double precision res, x1, y1, x2, y2

         x1 = 3
         x1 = 4 + x1
         y2 = x1
         y2 = x1 + y2
         y2 = y1 * x1 + 3
         y2 = y1 * x1 + 3 + y2
         res = y2
       end function
       
