       subroutine head()
         double precision w, x
         double precision y, z
         double precision a, b, c
c$openad INDEPENDENT(x)

c                      [u: x,w] [v: x]         [iA: x]
         a = x + w
c                      [u: a]   [v: x,a]       [iA: a]
         b = a
c                      [u: a,b] [v: x,a,b]     [iA: a,b]
         c = 2 + w
c                      [u: a,b] [v: x,a,b]     [iA: a,b]
         z = 4 - c + a
c                      [u: a,b] [v: x,a,b,z]   [iA: a,b]
         y = 3 * b + a
c                      [u: y]   [v: x,a,b,z,y] [iA: y]

c$openad DEPENDENT(y)
       end subroutine
