! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Field Access
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : No Issues
!
! Note: * Only StrictlyLocal memory references are Must, all other 
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

! Example :
! =========

         module myModule

            type repair_bill   
              real parts(20)   
              real labor   
            end type repair_bill   

         end module
  
         subroutine foo
            use myModule
 
            type(repair_bill) red_ferrari   
            real x

            x = red_ferrari%labor

         end subroutine


! Analysis :
! =========

          ! ======== AliasMapXAIF Results ============== 
          ! 
          !   [  MemRefHandle => SetId ]
          !   ===========================
          !
          ! MemRefHandle("X") => SetId(1)
          ! MemRefHandle("Red_Ferrari%labor") => SetId(2)
 
          !   [  SetId  =>  Virtual Address ]
          ! ================================== 
          ! SetId(1) => { LocTuple(4:4, Must) }
          ! SetId(2) => { LocTuple(5:5, May) }



