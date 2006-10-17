       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b

c                                                      Stmt
c                      [u: x]   [v: x]       [iA: x]
c         a = x                                        Act
c                      [u: a]   [v: x,a]     [iA: a]
c         b = a                                        Act
c                      [u: a,b] [v: x,a,b]   [iA: a,b]
c         y = 3 * b + a                                Act
c                      [u: y]   [v: x,a,b,y] [iA: y]
c
c ActiveLocs: nL(A), nL(B), iL(X), iL(Y)
c
c ActiveMemRefs: A,X,B,A,Y,A,B
c
c ActiveSyms: A,B


c$openad INDEPENDENT(x)

         a = x
         b = a
         y = 3 * b + a

c$openad DEPENDENT(y)
       end subroutine
