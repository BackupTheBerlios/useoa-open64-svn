! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Aliasing of actual parameters.
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status /Issues: No Issues
!
! Note:  To Date April 8th 2008, all memory references are "May".
!
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




         subroutine head(x,f) 
 
            double precision :: x 
            double precision :: f 
            double precision :: t1,t2,t3 
         
            t1 = x*f     
            call bar(t1,t1) 
            t3=f*30   
            f = t1+t2  
         end subroutine 

 
         subroutine bar(a,b) 
 
          double precision :: a,b 
 
          b=a 
 
         end subroutine 



! Analysis :
! =========

       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! MemRefHandle("T1") => SetId(1)
       ! MemRefHandle("X")  => SetId(2)
       ! MemRefhandle("F")  => SetId(3)
       ! MemRefHandle("T3") => SetId(4)
       ! MemRefHandle("F")  => SetId(3)
       ! MemRefHandle("F")  => SetId(3)
       ! MemRefHandle("T1") => SetId(1)
       ! MemRefHandle("T2") => SetId(5)
       ! MemRefHandle("B")  => SetId(6)
       ! MemRefHandle("A")  => SetId(6)
 
       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) =>  LocTuple(7:7, Must) 
       ! SetId(2) =>  LocTuple(16:16, May) 
       ! SetId(3) =>  LocTuple(17:17, May) 
       ! SetId(4) =>  LocTuple(11:11, Must) 
       ! SetId(5) =>  LocTuple(9:9, Must) 
       ! SetId(6) =>  LocTuple(6:6, May) 
       ! SetId(7) =>  LocTuple(7:7, May) 


 

