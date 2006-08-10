       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b
c$openad INDEPENDENT(x)

c                      [u: x]   [v: x]       [iA: x]
         a = x
c                      [u: a]   [v: x,a]     [iA: a]
         b = a
c                      [u: a,b] [v: x,a,b]   [iA: a,b]
         y = 3 * b + a
c                      [u: y]   [v: x,a,b,y] [iA: y]

c$openad DEPENDENT(y)
       end subroutine
