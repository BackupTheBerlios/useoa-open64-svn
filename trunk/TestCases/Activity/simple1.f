       subroutine head(x,y)
         double precision x
         double precision y
         double precision a
c                                                [stmt:]  
c                      [u: x] [v: x]     [iA: x]
c         a = x                                    Act
c                      [u: a] [v: x,a]   [iA: a]
c         y = 3 * a                                Act
c                      [u: y] [v: x,a,y] [iA: y]
c
c ActiveLocs: nL(A), iL(Y), iL(X)
c
c ActiveMemRefs: A, X, Y, A
c
c ActiveSym:     A (not X and Y because they are invLocs)



c$openad INDEPENDENT(x)

         a = x
         y = 3 * a

c$openad DEPENDENT(y)
       end subroutine
