!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves no aliasing due to reference
! parameters.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       ! AliasPairs:  (t1,*a) => 1, (t2,*b) => 2

       subroutine head(x, f) 
       integer g
       common /cpad/ g
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f               ! (t1,1), (x,2), (f,3)
       call bar(t1,t2)      ! (t1,1), (t2,4)
       t3=f*30              ! (t3,5), (f,3)
       f=t1+t2              ! (f,3), (t1,1), (t2,4)

       end subroutine

       subroutine bar(a,b)
       integer g
       common /cpad/ g
       double precision a,b

       b = a                ! (a,5), (b,6), (*a,1), (*b,4)
       return
       end
