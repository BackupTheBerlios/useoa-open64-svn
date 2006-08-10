      subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c
c$openad INDEPENDENT(x)

c                      [u: x]   [v: x]         [iA: x]
         a = x * 5
c                      [u: a]   [v: x,a]       [iA: a]
         b = a
c                      [u: a,b] [v: x,a,b]     [iA: a,b]
         if (a > 10) then
c                      [u: b]   [v: x,a,b]     [iA: b]
            c = 3
c                      [u: b,c] [v: x,a,b]     
         else
c                      [u: a,b] [v: x,a,b]     [iA: a,b]
            c = a * (-1)
c                      [u: b,c] [v: x,a,b,c]   
         end if
c                      [u: b,c] [v: x,a,b,c]   [iA: b,c]
         y = 3 * b + c
c                      [u: y]   [v: x,a,c,b,y] [iA: y]

c$openad DEPENDENT(y)
       end subroutine
