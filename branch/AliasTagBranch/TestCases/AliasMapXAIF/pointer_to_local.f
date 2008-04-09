! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Pointer pointing to local variables but static analysis
!              can not detect the exact location pointer is pointing at.
! 
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status /Issues: No Issues
!
! Note:  To Date April 8th 2008, all memory references are "May".!
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




! Example:
! ========

        program main 
 
         integer, pointer :: q 
         integer, target  :: r,s 
         integer          :: t 
 
         if ( t < 5 ) then   
              q=>r  
         else  
              q=>s    
         end if 

         t=q     

        end program 



! Analysis :
! =========

       ! ======== AliasTagFIAlias ================
       ! MemRefhandle("T")  => AliasTagResults(3, May)
       ! MemRefHandle("Q")  => AliasTagResults(1, May)
       ! MemRefHandle("Q")  => AliasTagResults(1, May)
       ! MemRefHandle("T")  => AliasTagResults(3, May)
       ! MemRefHandle("Q")  => AliasTagResults(2, May)

       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       !
       ! MemRefhandle("T")  => SetId(1)
       ! MemRefHandle("Q")  => SetId(2)
       ! MemRefHandle("Q")  => SetId(3)
       ! MemRefHandle("T")  => SetId(4)
       ! MemRefHandle("Q")  => SetId(5)

       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) => { LocTuple(3:3, May) }
       ! SetId(2) => { LocTuple(1:1, May) }
       ! SetId(3) => { LocTuple(1:1, May) }
       ! SetId(4) => { LocTuple(3:3, May) }
       ! SetId(5) => { LocTuple(2:2, May) }

