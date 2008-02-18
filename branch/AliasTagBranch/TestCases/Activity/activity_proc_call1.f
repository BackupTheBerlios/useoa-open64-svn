!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with X as the independent variable and F as the dependent
! t3 should not be active and t2 should
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3


c$openad INDEPENDENT(x)
!                       [u: x,f]   [v: x,t1]       [iA: x]                 
       t1=x*f
!                       [u: t1]    [v: x,t1]       [iA: t1]
       call bar(t1,t2)
!                       [u: t1,t2] [v: x,t1,t2]    [iA: t1,t2]
       t3=f*30
!                       [u: t1,t2] [v: x,t1,t2]    [iA: t1,t2]
       f=t1+t2
!                       [u: f]     [v: x,t1,t2,f]  [iA: f]  
c$openad DEPENDENT(f)

       end subroutine




       subroutine bar(a,b)
       double precision a,b
c                       [u: *a]    [v: *a]    [iA: *a]
       b = a
c                       [u: *a,*b] [v: *a,*b] [iA: *a,*b]
       return

       end subroutine



c* until we resolve the CALL_RETURN edge thingy, any usefuls below
c  a call will be useful above a call unless the useful gets def'd 
c  at the call statement (function ...)
c
c* until we resolve the CALL_RETURN edge thingy, any varys above
c  a call will be vary below a call unless the vary gets def'd
c  at the call statement (function ...) and there are no varys in
c  the actuals ??
c
c* why are call statements not allowed to be marked as active?
c

