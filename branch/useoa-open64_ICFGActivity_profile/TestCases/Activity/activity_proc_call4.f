!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with X as the independent variable and F as the dependent
! t3 should be active because specified as independent and t2 should
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

c$openad INDEPENDENT(x)
c$openad INDEPENDENT(t3)
c$openad DEPENDENT(f)
c                              [u: t2*,t3,x,f] [v: x,t3]         [iA: x,t3]
       t1=x*f
c                              [u: t1,t2*,t3]  [v: x,t1,t3]      [iA: t1,t3]
       call bar(t1,t2,t3,f)
c                              [u: t1,t2]      [v: x,t1,t2,t3,f] [iA: t1,t2]
       t3=f*30
c                              [u: t1,t2]      [v: x,t1,t2,t3,f] [iA: t1,t2]
       f=t1+t2
c                              [u: f]          [v: x,t1,t2,t3,f] [iA: f]

       end subroutine

       subroutine bar(a,b,c,d)
       double precision a,b,c,d

c                              [u: a,c]        [v: a,c]          [iA: a,c]
       b = a + c
c                              [u: a,b]        [v: a,b,c]        [iA: a,b]
       d = c
c                              [u: a,b]        [v: a,b,c,d]      [iA: a,b]
       return

       end

c* until we solve the CALL_RETURN edge thingy, all useful below a call
c  will be useful above a call, unless it is also an assign to a useful.
c
c* until we resolve the CALL_RETURN edge thingy, any varys above
c  a call will be vary below a call unless the vary gets def'd
c  at the call statement (function ...) and there are no varys in
c  the actuals ??
