       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c
c$openad INDEPENDENT(x)

c                      [useful: x]    [vary: x]
         a = x * 5
c                      [useful: a]    [vary: x, a]
         if (a > 10) then
c                      [useful: a]    [vary: x, a]
            c = a
c                      [useful: c, a] [vary: x, a, c]
         else
c                      [useful: a]    [vary: x, a]
            c = a * (-1)
c                      [useful: c, a] [vary: x, a, c]
         end if
c                      [useful: c, a] [vary: x, a, c]
         b = a
c                      [useful: b, c] [vary: x, a, c, b]
         y = 3 * b + c
c                      [useful: y]    [vary: x, a, c, b, y]

c$openad DEPENDENT(y)
       end subroutine
