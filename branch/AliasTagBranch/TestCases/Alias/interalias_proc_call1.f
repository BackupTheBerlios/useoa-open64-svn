!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing due to a reference parameter.
! PLM: I dont know if this is a valid program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       ! AliasPairs : (t1,*a,*b) => 1
      
       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f               ! (t1,1), (x,2), (f,3)
       call bar(t1,t1)      ! (t1,1), (t1,1)
       t3=f*30              ! (f,3), (t3,4)
       f=t1+t2              ! (f,3), (t1,1), (t2,5) 

       end subroutine

       subroutine bar(a,b)
       double precision a,b

       b = a                ! (a,6), (b,7), (*a,1), (*b,1)
       return
       end
