       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c
c$openad INDEPENDENT(x)

c                      [u: x]   [v: x]         [iA: x]
         a = x * 5
c                      [u: a]   [v: x,a]       [iA: a]
         if (a > 10) then
c                      [u: a]   [v: x,a]       [iA: a]
            c = a
c                      [u: c,a] [v: x,a,c]     
         else
c                      [u: a]   [v: x,a]       [iA: a]
            c = a * (-1)
c                      [u: c,a] [v: x,a,c]     
         end if
c                      [u: c,a] [v: x,a,c]     [iA: a,c]
         b = a
c                      [u: b,c] [v: x,a,c,b]   [iA: b,c]
         y = 3 * b + c
c                      [u: y]   [v: x,a,c,b,y] [iA: y]

c$openad DEPENDENT(y)
       end subroutine
