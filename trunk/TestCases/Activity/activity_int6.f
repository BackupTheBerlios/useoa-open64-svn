c       subroutine head(x,y)
       subroutine head()
         double precision x(2)
         double precision x1,x2
         double precision y(2)
         double precision y1,y2
         integer i
c$openad INDEPENDENT(x)
c                              [useful: x,y]
         i = 1
c                              [useful: x,y]
         x1 = 3
c                              [useful: x,y]
         x1 = 4 + x1
c                              [useful: x,y]
         y2 = x1
c                              [useful: x,y]
         y2 = x1 + y2
c                              [useful: x,y]
         x(1) = 5
c                              [useful: x,y]
         x(1) = 6 + x(1)
c                              [useful: x,y]
         x(2) = 7 + x(1)
c                              [useful: x,y]
         y(1) = x(1)
c                              [useful: x,y]
         y(2) = x(i)
c                              [useful: x,y]
         y(i) = x(2)
c                              [useful: x,y]
         y(1) = x(i+1)
c                              [useful: x,y]
         y(i) = y(i) + x(i)
c                              [useful: y]
         if (i < 2) then
c                              [useful: y]
           y2 = x1
c                              [useful: y]
         else
c                              [useful: y]
           y1 = x2
c                              [useful: y]
         end if
c                              [useful: y]
         y2 = y1 * x1 + 3
c                              [useful: y]
         y2 = y1 * x1 + 3 + y2
c                              [useful: y]
c$openad DEPENDENT(y)
       end subroutine

