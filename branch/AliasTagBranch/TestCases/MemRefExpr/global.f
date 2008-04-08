! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                        MemRefExpr TestCase:
!                        ====================
!       
! Features:  Testing MemRefExpr for Global Variable
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
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                               Analysis:
!                               =========
!
! Procedure foo:
!      - MemRefHandle(global_c) => NamedRef(def, main::global_c, 0)
!
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!                               Example :
!                               ==========

      program main

           double precision :: global_c
           common /c/global_c

      end program
      
      subroutine foo
           common /c/global_c

           global_c=20       

      end subroutine



