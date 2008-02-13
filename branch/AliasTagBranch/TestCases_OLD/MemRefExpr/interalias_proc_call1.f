!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing due to a reference parameter.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f
       call bar(t1,t1)
       t3=f*30
       f=t1+t2

       end subroutine

       subroutine bar(a,b)
       double precision a,b

       b = a  
       return
       end
