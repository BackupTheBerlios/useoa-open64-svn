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
c                      [useful: t2,x,f][vary: x]
       t1=x*f
c                      [useful: t1,t2*][vary: x,t1]
       call bar(t1,t2)
c                      [useful: t1,t2] [vary: x,t1,t2]
       t3=f*30
c                      [useful: t1,t2] [vary: x,t1,t2]
       f=t1+t2
c                      [useful: f]     [vary: x,t1,t2,f]
c$openad DEPENDENT(f)

       end subroutine

       subroutine bar(a,b)
       double precision a,b
c                      [useful: a]   [vary: a]
       b = a  
c                      [useful: a,b] [vary: a,b]
       return
c                      [useful: a,b] [vary: a,b]
       end

c* until we resolve the CALL_RETURN edge thingy, any usefuls below
c  a call will be useful above a call unless the useful gets def'd 
c  at the call statement (function ...)
