       subroutine head(x,y)
         double precision x
         double precision y
         double precision a
c$openad INDEPENDENT(x)
c                      [u: x] [v: x] [iA: x]
         a = x
c                      [u: a] [v: a] [iA: a]
         y = 3 * a
c                      [u: y] [v: y] [iA: y]
c$openad DEPENDENT(y)
       end subroutine
