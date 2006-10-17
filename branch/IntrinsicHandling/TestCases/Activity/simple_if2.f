      subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c

c                                                                [Stmt:]
c                   [u: x]   [v: x]         [iA: x]
c   a = x * 5                                                     Act
c                   [u: a]   [v: x,a]       [iA: a]
c   b = a                                                         Act
c                   [u: a,b] [v: x,a,b]     [iA: a,b]
c   if (a > 10) then                                              Passive
c                   [u: b]   [v: x,a,b]     [iA: b]
c      c = 3                                                      Act*
c                   [u: b,c] [v: x,a,b]               [oA: b]
c   else
c                   [u: a,b] [v: x,a,b]     [iA: a,b]
c      c = a * (-1)                                               Act
c                   [u: b,c] [v: x,a,b,c]             [oA: b,c]
c   end if
c                   [u: b,c] [v: x,a,b,c]   [iA: b,c]
c   y = 3 * b + c                                                 Act
c                   [u: y]   [v: x,a,c,b,y] [iA: y]
c
c ActiveLocs: nL(A),nL(B),nL(C),iL(X),iL(Y)
c
c ActiveStmts: ERROR:  (c = 3) is marked Active and should not be.
c              This is due to using the inActive set from the successor
c              statement as the outActive set for (c = 3).  Since the
c              outVary sets from (c = 3) and (c = a * (-1)) merge to 
c              give the inVary set of [v: x,a,b,c] for stmt (y = 3 * b + c)
c              and the intersection with the useful set of [u: b,c] leads
c              to an inActive set of [iA: b,c] for the successor statement,
c              the ICFGActive::transfer uses this set as the outActive for
c              (c = 3) rather than the accurate [oA: b] set and incorrectly
c              marks it as an active statement.
c
c ActiveMemRefs: A,X,B,A,C,C*,A,Y,C,B: ERROR: C* from (c = 3) statement
c                                             is incorrectly marked due to
c                                             the ActiveStmt error above
c ActiveSyms: A,B,C
c


c$openad INDEPENDENT(x)
         a = x * 5
         b = a
         if (a > 10) then
            c = 3
         else
            c = a * (-1)
         end if
         y = 3 * b + c
c$openad DEPENDENT(y)
       end subroutine
