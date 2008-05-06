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
! Note: * Only strictlyLocal memory references are Must, all other 
!         memory references are "May".
!
!       * All the MemRefHandles for which AliasTagsSet is empty
!          are Mapped to AliasMapXAIF setId = 0, [jean's Suggestion, 
!          April 2008].
!
!
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
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

       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       !
       ! MemRefhandle("T")  => SetId(1)
       ! MemRefHandle("Q")  => SetId(2)
       ! MemRefHandle("Q")  => SetId(2)
       ! MemRefHandle("T")  => SetId(1)
       ! MemRefHandle("Q")  => SetId(3)
       ! MemRefHandle(&R)   => SetId(0)
       ! MemRefHandle(&S)   => SetId(0)   

       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(0) => { }
       ! SetId(1) => { LocTuple(7:7, Must) }
       ! SetId(2) => { LocTuple(2:2, Must) }
       ! SetId(3) => { LocTuple(3:3, May) }
       !             { LocTuple(4:4, May) }
       !             { LocTuple(5:5, May) }

