!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that passes a reference parameter as a reference param.
!
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
       call bar(x,g)
       t3=f*30
       f=t1+t2

       end subroutine

       subroutine bar(a,b)
       common /cpad/ g
       double precision a,b

       b = a + g 
       a = 3 

       return
       end

