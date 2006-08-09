       subroutine head()
         double precision w, x
         double precision y, z
         double precision a, b, c
c$openad INDEPENDENT(x)

c                      [useful: x,w] [vary: x]         => [active: x]
         a = x + w
c                      [useful: a]   [vary: x,a]       => [active: a]
         b = a
c                      [useful: a,b] [vary: x,a,b]     => [active: a,b]
         c = 2 + w
c                      [useful: a,b] [vary: x,a,b]     => [active: a,b]
         z = 4 - c + a
c                      [useful: a,b] [vary: x,a,b,z]   => [active: a,b]
         y = 3 * b + a
c                      [useful: y]   [vary: x,a,b,z,y] => [active: y]

c$openad DEPENDENT(y)
       end subroutine
