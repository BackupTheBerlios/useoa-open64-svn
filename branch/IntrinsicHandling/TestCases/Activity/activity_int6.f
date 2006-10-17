c       subroutine head(x,y)
       subroutine head()
         double precision x(2)
         double precision x1,x2
         double precision y(2)
         double precision y1,y2
         integer i
c$openad INDEPENDENT(x)
c                              [u: x,y] [v: x]   [iA: x]
         i = 1
c                              [u: x,y] [v: x]   [iA: x]
         x1 = 3
c                              [u: x,y] [v: x]   [iA: x]
         x1 = 4 + x1
c                              [u: x,y] [v: x]   [iA: x]
         y2 = x1
c                              [u: x,y] [v: x]   [iA: x]
         y2 = x1 + y2
c                              [u: x,y] [v: x]   [iA: x]
         x(1) = 5
c                              [u: x,y] [v: x]   [iA: x]
         x(1) = 6 + x(1)
c                              [u: x,y] [v: x]   [iA: x]
         x(2) = 7 + x(1)
c                              [u: x,y] [v: x]   [iA: x]
         y(1) = x(1)
c                              [u: x,y] [v: x,y] [iA: x,y]
         y(2) = x(i)
c                              [u: x,y] [v: x,y] [iA: x,y]
         y(i) = x(2)
c                              [u: x,y] [v: x,y] [iA: x,y]
         y(1) = x(i+1)
c                              [u: x,y] [v: x,y] [iA: x,y]
         y(i) = y(i) + x(i)
c                              [u: y]   [v: x,y] [iA: y]
         if (i < 2) then
c                              [u: y]   [v: x,y] [iA: y]
           y2 = x1
c                              [u: y]   [v: x,y] [iA: y]
         else
c                              [u: y]   [v: x,y] [iA: y]
           y1 = x2
c                              [u: y]   [v: x,y] [iA: y]
         end if
c                              [u: y]   [v: x,y] [iA: y]
         y2 = y1 * x1 + 3
c                              [u: y]   [v: x,y] [iA: y]
         y2 = y1 * x1 + 3 + y2
c                              [u: y]   [v: x,y] [iA: y]
c$openad DEPENDENT(y)
       end subroutine

