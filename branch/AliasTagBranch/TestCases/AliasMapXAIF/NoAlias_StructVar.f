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
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



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


          ! ========= AliasTagFIAlias Results =========
          !
          !   [ MemRefHandle  =>  AliasTag]
          !   =============================
          !   MemRefHandle(RED_FERRARI%LABOR) => ((2,4), May)    
          !   MemRefHandle(BLACK_BERRY)       => (1, May)
          !   MemRefHandle(X:0:.predef_F4)    => (3, May)


          ! ======== AliasMapXAIF Results ================
          !
          !   [  MemRefHandle => SetId ]
          !   ===========================
          !   MemRefHandle(RED_FERRARI%LABOR) => SetId(1)
          !   MemRefHandle(BLACK_BERRY)       => SetId(2)
          !   MemRefHandle(X:0:.predef_F4)    => SetId(3)
          !
          !   [  SetId  =>  Virtual Address ]
          ! ==================================
          ! SetId(1) => { LocTuple(2:2, May),
          !               LocTuple(4:4, May) }
          ! SetId(2) => { LocTuple(1:1, May}
          ! SetId(3) => { LocTuple(3:3, May) }


