!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with X as the independent variable and F as the dependent
! t3 should not be active and t2 should
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine biggerhead(x,f,g)
       double precision :: xbg, fbg, gbg

c                             [useful: xbg,fbg]
       call head (xbg,fbg)
c                             [useful: ]
       end subroutine

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

c$openad INDEPENDENT(x)
c                             [useful: t2*,x,f]
       t1=x*f
c                             [useful: t1,t2*]
       call bar(t1,t2)
c                             [useful: t1,t2]
       t3=f*30
c                             [useful: t1,t2]
       f=t1+t2
c                             [useful: f]
c$openad DEPENDENT(f)

       end subroutine

       subroutine bar(a,b)
       double precision a,b

c                             [useful: a]
       b = a  
c                             [useful: a,b]
       return
c                             [useful: a,b]
       end
