       subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c

c                                                             [stmt:]
c                   [u: x]   [v: x]         [iA: x]
c   a = x * 5                                                  Act
c                   [u: a]   [v: x,a]       [iA: a]
c   if (a > 10) then                                           Passive
c                   [u: a]   [v: x,a]       [iA: a]
c      c = a                                                   Act
c                   [u: c,a] [v: x,a,c]             [oA: a,c]
c   else                                                      
c                   [u: a]   [v: x,a]       [iA: a]
c      c = a * (-1)                                            Act
c                   [u: c,a] [v: x,a,c]             [oA: a,c]
c   end if
c                   [u: c,a] [v: x,a,c]     [iA: a,c]
c   b = a                                                      Act
c                   [u: b,c] [v: x,a,c,b]   [iA: b,c]
c   y = 3 * b + c                                              Act
c                   [u: y]   [v: x,a,c,b,y] [iA: y]
c
c
c ActiveLocs: nL(A),nL(B),nL(C),iL(X),iL(Y)
c
c ActiveMemRefs: A,X,C,A,C,A,B,A,Y,C,B
c
c ActiveSyms: A,B,C


c$openad INDEPENDENT(x)
         a = x * 5
         if (a > 10) then
            c = a
         else
            c = a * (-1)
         end if
         b = a
         y = 3 * b + c
c$openad DEPENDENT(y)
       end subroutine
