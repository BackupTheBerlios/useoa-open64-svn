!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f
       call bar(t1,t2)
       t3=f*30
       call bar(t3,t2)
       f=t1+t2

       end subroutine

       subroutine bar(a,b)
       double precision a,b

       b = a  
       return
       end
