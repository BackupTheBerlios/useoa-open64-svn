!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with X as the independent variable and F as the dependent
! t3 should not be active and t2 should
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine biggerhead(x,f,g)
       double precision :: xbg, fbg, gbg

       call head (xbg,fbg)
       end subroutine

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

c$openad INDEPENDENT(x)
       t1=x*f
       call bar(t1,t2)
       t3=f*30
       f=t1+t2
c$openad DEPENDENT(f)

       end subroutine

       subroutine bar(a,b)
       double precision a,b

       b = a  
       return
       end
