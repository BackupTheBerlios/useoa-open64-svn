!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing of a reference parameter to a 
! global variable.
!
! Want to see the reachdef(s) for a=b.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       program testing
       double precision g
       common /cpad/ g
       double precision t1, t2
       call head(t1, t2)
       end

       subroutine head(x, f) 
       common /cpad/ g
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f
       call bar(t1,g)
       t3=f*30
       f=t1+t2

       end subroutine

       subroutine bar(a,b)
       common /cpad/ g
       double precision a,b

       b = a + g 
       g = 1
       a = b
       return
       end

