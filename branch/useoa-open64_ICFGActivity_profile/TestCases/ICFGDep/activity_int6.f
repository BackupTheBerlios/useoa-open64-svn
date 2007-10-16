c       subroutine head(x,y)
       subroutine head()
         double precision x(2)
         double precision x1,x2
         double precision y(2)
         double precision y1,y2
         integer i
c$openad INDEPENDENT(x)
         i = 1
         x1 = 3
         x1 = 4 + x1
         y2 = x1
         y2 = x1 + y2
         x(1) = 5
         x(1) = 6 + x(1)
         x(2) = 7 + x(1)
         y(1) = x(1)
         y(2) = x(i)
         y(i) = x(2)
         y(1) = x(i+1)
         y(i) = y(i) + x(i)

         if (i < 2) then
           y2 = x1
         else
           y1 = x2
         end if
         y2 = y1 * x1 + 3
         y2 = y1 * x1 + 3 + y2
c$openad DEPENDENT(y)
       end subroutine

