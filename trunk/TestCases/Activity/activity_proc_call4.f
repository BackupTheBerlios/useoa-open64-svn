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
c                              [useful: t2*,t3,x,f]
       t1=x*f
c                              [useful: t1,t2*,t3]
       call bar(t1,t2,t3,f)
c                              [useful: t1,t2]
       t3=f*30
c                              [useful: t1,t2]
       f=t1+t2
c                              [useful: f]

       end subroutine

       subroutine bar(a,b,c,d)
       double precision a,b,c,d

c                              [useful: a,c]
       b = a + c
c                              [useful: a,b]
       d = c
c                              [useful: a,b]
       return

       end

c* until we solve the CALL_RETURN edge thingy, all useful below a call
c  will be useful above a call, unless it is also an assign to a useful.
