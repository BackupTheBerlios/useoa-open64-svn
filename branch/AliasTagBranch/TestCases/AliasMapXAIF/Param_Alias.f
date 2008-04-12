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

       ! ======== AliasTagFIAlias ==================
       ! MemRefHandle("T1") => AliasTagResult(4, May)
       ! MemRefHandle("X")  => AliasTagResult(9, May)
       ! MemRefhandle("F")  => AliasTagResult(10, May)
       ! MemRefHandle("T3") => AliasTagResult(6 May)
       ! MemRefHandle("F")  => AliasTagResult(10, May)
       ! MemRefHandle("F")  => AliasTagResult(10, May)
       ! MemRefHandle("T1") => AliasTagResult(4, May)
       ! MemRefHandle("T2") => AliasTagResult(5, May)
       ! MemRefHandle("B")  => AliasTagResult(4, May)
       ! MemRefHandle("A")  => AliasTagResult(4, May)

       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! MemRefHandle("T1") => SetId(1)
       ! MemRefHandle("X")  => SetId(2)
       ! MemRefhandle("F")  => SetId(3)
       ! MemRefHandle("T3") => SetId(4)
       ! MemRefHandle("F")  => SetId(5)
       ! MemRefHandle("F")  => SetId(6)
       ! MemRefHandle("T1") => SetId(7)
       ! MemRefHandle("T2") => SetId(8)
       ! MemRefHandle("B")  => SetId(9)
       ! MemRefHandle("A")  => SetId(10)
 
       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) =>  LocTuple(4:4, May) 
       ! SetId(2) =>  LocTuple(9:9, May) 
       ! SetId(3) =>  LocTuple(10:10, May) 
       ! SetId(4) =>  LocTuple(6:6, May) 
       ! SetId(5) =>  LocTuple(10:10, May) 
       ! SetId(6) =>  LocTuple(10:10, May) 
       ! SetId(7) =>  LocTuple(4:4, May) 
       ! SetId(8) =>  LocTuple(5:5, May) 
       ! SetId(9) =>  LocTuple(4:4, May) 
       ! SetId(10)=>  LocTuple(4:4, May) 


 

