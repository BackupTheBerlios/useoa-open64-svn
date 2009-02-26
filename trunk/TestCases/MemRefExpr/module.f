! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   MemRefExpr
!                   ============
!       
! Features:  Testing MemRefExpr for Module structure
!
!            NamedRef(MemRefType, SymHandle, StrictlyLocal)
!               MemRefType    => def/use
!               strictlyLocal => { 0  | NonLocal to the procedure P}
!                                { 1  | Local to the procedure P}
!
! Testing :
!          [X] MemRefExpr Type (Def/Use)
!          [X] StrictlyLocal = 0
!
! Status : No Issues
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!                    Analysis:
!                    =========
!
! Procedure foo:
!      - MemRefHandle first%labor => FieldAccess(NamedRef(def, global::first, 0), labor)
!
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
!
!
!                     Example : 
!                     =========

      module global

           type repair_bill
              real parts(20)
              real labor
              real pointer insurance
          end type repair_bill

          type(repair_bill), target :: first

      end module

      subroutine foo
           use global

           first%labor = 10.0   

      end subroutine


