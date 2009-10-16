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
!c$openad INDEPENDENT(L) 
! 
!                              [U: N, temp, L]  [V: L]              [A: L] 
!      CALL SUB1(L+1)                    
!                              [U: N]           [V: L, temp, N]     [A: N]   // temp=L+1 
!      X=N 
!                              [U: X]           [V: L, temp, N, X]  [A: X] 
!c$openad DEPENDENT(X) 
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

! =====================================================================
!
!                 * ICFGCSActivity Analysis after CSAlias change (10/14/09)
!                 =========================
!
!      PROGRAM MAIN 
!      INTEGER N,X,L 
!      COMMON /GLOBALS/ N 
!
!c$openad INDEPENDENT(L)  
!                        [U: L<null>]     [V: L]              [iA: L] 
!
!      //CALL SUB(L+1) becomes:
!      temp=L+1 // bundled as an assignPair with call, results below unseen
!                        [U: temp<ch(0)>] [V: L, temp]        [iA: temp]
!      CALL SUB1(temp)                    
!                        [U: N<null>]     [V: L, temp, N]     [iA: N]
!      X=N 
!                        [U: X<null>]     [V: L, temp, N, X]  [iA: X] 
!c$openad DEPENDENT(X) 
!      END 
! 
!      SUBROUTINE SUB1(F) 
!      INTEGER N,F 
!      COMMON /GLOBALS/ N 
!                        [U: *F<SUB1(L+1)>]  [V: L, *F]       [iA: *F] 
!      N=F 
!                        [U: N<null>]        [V: L, *F, N]    [iA: N] 
!      END 
! =====================================================================
!
!                 * ICFGCSActivity Analysis before CSAlias change
!                 =========================
!
!      PROGRAM MAIN 
!      INTEGER N,X,L 
!      COMMON /GLOBALS/ N 
!
!c$openad INDEPENDENT(L)  
!                        [U: N<CH(0)>+#]  [V: L]                      [iA: ] 
!      //temp=L+1 
!      CALL SUB1(L+1)                    
!                        [U: N<CH(0)>]    [V: L, temp, N<SUB1(L+1)>]  [iA: ]
!      X=N 
!                        [U: X]           [V: L, temp, N<SUB1(L+1)>]  [iA: ] 
!c$openad DEPENDENT(X) 
!      END 
! 
!      SUBROUTINE SUB1(F) 
!      INTEGER N,F 
!      COMMON /GLOBALS/ N 
!                        [U: N<CH(0)>#]   [V: *F, L]                  [iA: ] 
!      N=F 
!                        [U: N<CH(0)>#]   [V: *F, L, N<SUB1(L+1)>]    [iA: ] 
!      END 
!
! -----------------------------
! ICFGCSActivity Notes 10/12/09 -- CSAlias is not handling globals correctly
! ----------------------------------------------------------------------
! + Global N should not be Useful here, as it should get def'd (i.e. Kill'd)
!     in the Call SUB1(L+1).  CSAlias is having a problem with Globals ...
! # Consequently, since the global N<SUB1(F)> is not recognized as an alias
!     to global N<CH(0)>, it is not considered useful in the call to SUB1
!     and thus F cannot become useful, and also neither temp nor L
!
! ======================================================================


