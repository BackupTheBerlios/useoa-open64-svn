!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing of a reference parameter to a 
! global variable.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       program testing
       double precision g,g1
       common /cpad/ g,g1
       double precision t1, t2
       call head(t1, t2)
       end

       subroutine head(x, f) 
       common /cpad/ g,g1
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f
       g=1.0
       g1=2.0
       call bar(t1,g1,g)
       t3=f*30*g
       f=t1+t2

       end subroutine

       subroutine bar(a,b,c)
       common /cpad/ g,g1
       double precision a,b,c

       b = a + g + g1 + c
       return
       end

