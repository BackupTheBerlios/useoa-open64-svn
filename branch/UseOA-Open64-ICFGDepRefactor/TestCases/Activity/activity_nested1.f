!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with X as the independent variable and F as the dependent
! t3 should not be active and t2 should
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine biggerhead(x,f,g)
       double precision :: xbg, fbg, gbg

c                             [u: xbg,fbg] [v: ]          [iA: ]
       call head (xbg,fbg)
c                             [u: ]        [v: x,f]       [iA: ]
       end subroutine

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

c$openad INDEPENDENT(x)
c                             [u: t2*,x,f] [v: x]         [iA: x]
       t1=x*f
c                             [u: t1,t2*]  [v: x,t1]      [iA: t1]
       call bar(t1,t2)
c                             [u: t1,t2]   [v: x,t1,t2]   [iA: t1,t2]
       t3=f*30
c                             [u: t1,t2]   [v: x,t1,t2]   [iA: t1,t2]
       f=t1+t2
c                             [u: f]       [v: x,t1,t2,f] [iA: f]
c$openad DEPENDENT(f)

       end subroutine

       subroutine bar(a,b)
       double precision a,b

c                             [u: a]       [v: a]         [iA: a]
       b = a  
c                             [u: a,b]     [v: a,b]       [iA: a,b]
       return

       end
