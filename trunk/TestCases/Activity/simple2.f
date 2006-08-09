       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b
c$openad INDEPENDENT(x)

c                      [useful: x]    [vary: x]
         a = x
c                      [useful: a]    [vary: x, a]
         b = a
c                      [useful: a, b] [vary: x, a, b]
         y = 3 * b + a
c                      [useful: y]    [vary: x, a, b, y]

c$openad DEPENDENT(y)
       end subroutine
