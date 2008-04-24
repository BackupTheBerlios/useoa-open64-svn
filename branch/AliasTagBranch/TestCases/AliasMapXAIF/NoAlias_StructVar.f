! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features: 
!           1. Struct Variable Assignment
!           2. FieldAccess
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags 
!
! Status : No Issues
!
! Note: Both red_ferrari and black_berry are structure variables.
!       but red_ferrari is involved in the fieldAccess operation and 
!       therefore gets multiple AliasTags (indicates red_ferrari and
!       red_ferrari%labor overlap).
!
! Note:  To Date April 8th 2008, all memory references are "May".
!
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem: Not sure why red_ferrari gets 3 Tags
! ==============================================

! Example : 
! ==========

         module myModule

            type repair_bill   
                real labor   
                integer pnumber
            end type repair_bill   

         end module
  
         subroutine foo

            use myModule
            type(repair_bill) red_ferrari, black_berry, black_berry
            real x

            red_ferrari = black_berry        ! Structure Variable Assignment
            x = red_ferrari%labor            ! FieldAccess

          end subroutine  



! Analysis :
! =========


          ! ======== AliasMapXAIF Results ================
          !
          !   [  MemRefHandle => SetId ]
          !   ===========================
          !   MemRefHandle(RED_FERRARI)       => SetId(1)
          !   MemRefHandle(BLACK_BERRY)       => SetId(2)
          !   MemRefHandle(X:0:.predef_F4)    => SetId(3)
          !   MemRefHandle(RED_FERRARI%LABOR) => SetId(4)
          !
          !   [  SetId  =>  Virtual Address ]
          ! ==================================
          ! SetId(1) => { LocTuple(3:3, May),
          !               LocTuple(4:4, May),
          !               LocTuple(7:7, May) }
          ! SetId(2) => { LocTuple(2:2, Must) }
          ! SetId(2) => { LocTuple(6:6, Must}
          ! SetId(3) => { LocTuple(7:7, May) }


