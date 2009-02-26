!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves no aliasing due to reference
! parameters.
! t3 is inactive, activeGlobal is active, and passiveGlobal is not
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision activeGlobal
       double precision passiveGlobal
       common /cpad/ activeGlobal, passiveGlobal
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
       double precision activeGlobal
       double precision passiveGlobal
       common /cpad/ activeGlobal, passiveGlobal
       double precision a,b

       activeGlobal = a * 2
       b = a  + activeGlobal + passiveGlobal
       return
       end
