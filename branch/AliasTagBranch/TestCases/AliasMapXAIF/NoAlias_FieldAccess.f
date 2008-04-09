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
! Note: To Date April 8th 2008, all memory references are "May".
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

         ! ========= AliasTagFIAlias Results =========
         !
         ! MemRefHandle("X") => (2, May)
         ! MemRefHandle("Red_Ferrari%labor") => 3
  

          ! ======== AliasMapXAIF Results ============== 
          ! 
          !   [  MemRefHandle => SetId ]
          !   ===========================
          !
          ! MemRefHandle("X") => SetId(1)
          ! MemRefHandle("Red_Ferrari%labor") => SetId(2)
 
          !   [  SetId  =>  Virtual Address ]
          ! ================================== 
          ! SetId(1) => { LocTuple(2:2, May) }
          ! SetId(2) => { LocTuple(3:3, May) }



