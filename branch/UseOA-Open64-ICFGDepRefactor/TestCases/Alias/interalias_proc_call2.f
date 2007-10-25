!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves no aliasing due to reference
! parameters.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       integer g
       common /cpad/ g
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f
       call bar(t1,t2)
       t3=f*30
       f=t1+t2

       end subroutine

       subroutine bar(a,b)
       integer g
       common /cpad/ g
       double precision a,b

       b = a  
       return
       end
