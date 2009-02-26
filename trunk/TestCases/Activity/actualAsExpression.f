! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:
!          [X] expressions as actual.
!
! Testing :
!          [X] ActiveStmts           :  All Statements are active
!          [X] ActiveTags/MemRefExprs:  L, L+1, X, N, *F
!          [X] ActiveSyms            :  L, X, N, F
!
!
! Note:    Except strictlyLocal all definitions are mayDefs.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      PROGRAM MAIN 
      INTEGER N,X,L 
      COMMON /GLOBALS/ N 
c$openad INDEPENDENT(L) 
 
      CALL SUB1(L+1)                    
      X=N 

c$openad DEPENDENT(X) 
      END 
 
      SUBROUTINE SUB1(F) 
      INTEGER N,F 
      COMMON /GLOBALS/ N 
      N=F 
      END 
 



! ===================================================================
!
!                  * Intutive Analysis
!                  ====================
!
!  Intutive analysis first flatten the procedure calls. Then using
!  manual analysis, we are trying to find out value flow path between
!  Independent and Dependent variables.
!
!  Below shows the Intutive analysis result using 3-tuple information
!  [Program Point(PP), stmt, Path]
!
!  We explored all possible paths from independent variable. In order
!  to avoid confusion, we keep track of each and every path seperately.
!
!  please consider that L, F indicate symbols and program
!  point at which they are defined. This helps us to keep track of
!  recent definition along various paths under examination.
!
!
! CALL SUB1(L+1)       L=>temp=>*F
! N=F                  L=>temp=>*F=>N
! X=N                  L=>temp=>*F=>N=>X       pactive path[
!
! =====================================================================
!
!                 * ICFGActivity Analysis
!                 =======================
!
!      PROGRAM MAIN 
!      INTEGER N,X,L 
!      COMMON /GLOBALS/ N 
! 
!                              [U: N, temp, L]  [V: L]              [A: L] 
!      CALL SUB1(L+1)                    
!                              [U: N]           [V: L, temp, N]     [A: N]   // temp=L+1 
!      X=N 
!                              [U: X]           [V: L, temp, N, X]  [A: X] 
!      END 
! 
!      SUBROUTINE SUB1(F) 
!      INTEGER N,F 
!      COMMON /GLOBALS/ N 
!                              [U: N,*F]       [V: *F]             [A: *F] 
!      N=F 
!                              [U: N]          [V: N, *F]          [A: N] 
!      END 
! 

