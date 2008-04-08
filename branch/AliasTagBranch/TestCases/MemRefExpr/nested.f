! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   MemRefExpr
!                   ============
! 
! Features:  Nested Procedures.
!
!            1. NamedRef(MemRefType, SymHandle, StrictlyLocal)
!               MemRefType    => def/use
!               strictlyLocal => { 0  | NonLocal to the procedure P}
!                                { 1  | Local to the procedure P}
!
!
! Testing :
!          [X] StrictlyLocal declared in the outermost proc and its
!              visibility in the innermost proc.
!
! Status : No Issues
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   Analysis :
!                   ===========

! Procedure main:
!             - MemRefHandle b => NamedRef(def, main::b, 1)
!             - MemRefHandle a => NamedRef(use, main::a, 1)

! Procedure Sub1:
!             - MemRefHandle c => NamedRef(def, sub1::c, 1)
!             - MemRefHandle a => NamedRef(use, main::a, 0)
!
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   Example :
!                   =========

        ! ===== OuterMost Procedure =====

        program main 
 
            double precision :: a,b 
            b=a 
            contains 

            ! ===== Innermost Procedure =====
 
            subroutine sub1 () 
               double precision :: c 
               c=a 
            end subroutine sub1 
 
        end program main 




