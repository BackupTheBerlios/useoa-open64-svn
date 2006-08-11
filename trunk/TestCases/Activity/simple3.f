       subroutine head(w,x,y,z)
c  w and x are INTENT::IN 
c  y and z are INTENT::OUT 
         double precision w, x
         double precision y, z
         double precision a, b, c

c                                                         [Stmt:]
c                      [u: x,w] [v: x]         [iA: x]
c         a = x + w                                        Act
c                      [u: a]   [v: x,a]       [iA: a]
c         b = a                                            Act
c                      [u: a,b] [v: x,a,b]     [iA: a,b]
c         c = 2 + w                                        Passive
c                      [u: a,b] [v: x,a,b]     [iA: a,b]
c         z = 4 - c + a                                    Passive
c                      [u: a,b] [v: x,a,b,z]   [iA: a,b]
c         y = 3 * b + a                                    Act
c                      [u: y]   [v: x,a,b,z,y] [iA: y]
c
c ActiveLocs: nL(A), nL(B), iL(X), iL(Y)
c
c ActiveMemRefs: A,X,B,A,Y,A,B
c
c ActiveSyms: A,B

c$openad INDEPENDENT(x)
         a = x + w
         b = a
         c = 2 + w
         z = 4 - c + a
         y = 3 * b + a
c$openad DEPENDENT(y)
       end subroutine
