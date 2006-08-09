       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c
c$openad INDEPENDENT(x)

c                      [useful: x]    [vary: x]
         a = x * 5
c                      [useful: a]    [vary: x, a]
         b = a
c                      [useful: a,b]  [vary: x, a, b]
         if (a > 10) then
c                      [useful: b]    [vary: x, a, b]
            c = 3
c                      [useful: b,c]  [vary: x, a, b]
         else
c                      [useful: a,b]  [vary: x, a, b]
            c = a * (-1)
c                      [useful: b,c]  [vary: x, a, b, c]
         end if
c                      [useful: b,c]  [vary: x, a, b, c]
         y = 3 * b + c
c                      [useful: y]    [vary: x, a, c, b, y]

c$openad DEPENDENT(y)
       end subroutine
