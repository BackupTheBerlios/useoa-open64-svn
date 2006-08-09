       subroutine head(x,y)
         double precision x
         double precision y
         double precision a
c$openad INDEPENDENT(x)
c                      [useful: x]
         a = x
c                      [useful: a]
         y = 3 * a
c                      [useful: y]
c$openad DEPENDENT(y)
       end subroutine
