! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Pointer Aliasing with local variables
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status /Issues: No Issues
!
! Note   :
!         * To Date April 8th 2008, all memory references are "May".
!         
!         * Following Analysis does not take into consideration of 
!          srictly local modification algorithm. For the reference
!          I would write what Michelle and I discussed in the meeting
!          
!          AliasTag Results with Strictly Local Modiciations 
!          for the following example
!          - MemRefHandle(q) => (2, Must)
!          - MemRefHandle(*p) =>  ((2,3), May)
!         
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! Example :
! ==========

        program foo 
 
         integer, pointer :: p 
         integer, target  :: q 
         integer          :: t 
 
         p=>q            
         t=p   
  
       end program 




! Analysis :
! =========

       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! 
       ! MemRefHandle(P)  => SetId(1)
       ! MemRefHandle(T)  => SetId(2)
       ! MemRefHandle(P)  => SetId(3)

       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) => { LocTuple(2:2, Must) }
       ! SetId(2) => { LocTuple(6:6, Must) }
       ! SetId(3) => { LocTuple(3:3, May) 
       !               LocTuple(4:4, May) }

