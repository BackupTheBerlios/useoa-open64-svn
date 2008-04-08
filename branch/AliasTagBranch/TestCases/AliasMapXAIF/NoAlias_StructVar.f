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
          !   [ MemRefExpr  =>  AliasTag ]
          !   ============================
          ! NamedRef("red_ferrari")           => ((1,2), Must)
          ! NamedRef("black_berry")           => (3, Must)
          ! NamedRef("red_ferrari%labor")     => ((2,2), May)
          !
          !
          !   [ MemRefHandle  =>  AliasTag]
          !   =============================
          ! MemRefHandle("red_ferrari")       => ((1,2), Must)
          ! MemRefHandle("black_berry")       => (3, Must)
          ! MemRefHandle("red_ferrari%labor") => ((2,2), May)





          ! ======== AliasMapXAIF Results ================
          !
          !   [  MemRefHandle => SetId ]
          !   ===========================
          ! MemRefHandle("red_ferrari")        => SetId(1)
          ! MemRefHandle("black_berry")        => SetId(2)
          ! MemRefHandle("red_ferrari%labor")  => SetId(3)
          !
          !
          !   [  SetId  =>  Virtual Address ]
          ! ==================================
          ! SetId(1) => { LocTuple(1:1, May),
          !               LocTuple(2:2, May) }
          ! SetId(2) => { LocTuple(3:3, May}
          ! SetId(3) => { LocTuple(2:2, May) }


