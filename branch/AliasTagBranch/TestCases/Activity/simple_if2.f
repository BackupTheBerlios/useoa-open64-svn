! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:
!           1. Conditional assignment, i.e. variable is active
!              along one path and notactive along other path
!              but set to active due to flow-sensitivity.
!              [test of Flow-Sensitivity]
!
! Testing :
!
! [X] ActiveStmts: ERROR:  (c = 3) is marked Active and should not be.
!              This is due to using the inActive set from the successor
!              statement as the outActive set for (c = 3).  Since the
!              outVary sets from (c = 3) and (c = a * (-1)) merge to
!              give the inVary set of [v: x,a,b,c] for stmt (y = 3 * b + c)
!              and the intersection with the useful set of [u: b,c] leads
!              to an inActive set of [iA: b,c] for the successor statement,
!              the ICFGActive::transfer uses this set as the outActive for
!              (c = 3) rather than the accurate [oA: b] set and incorrectly
!              marks it as an active statement.
!
! [X] ActiveMemRefExprs: *x,a,b,c,*y
!       
! [X] ActiveSyms:  x,a,b,c,y
!
! Status : I dont know why NamedRef("a") shows up in the output of
!          ICFGActivity analysis.
!
! Note:    All Definitions are MayDefs
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!             * Example
!             ==========             

      subroutine head(x,y)
         double precision x
         double precision y
         double precision a, b, c

c$openad INDEPENDENT(x)
         a = x * 5
         b = a
         if (a > 10) then
            c = 3
         else
            c = a * (-1)
         end if
         y = 3 * b + c
c$openad DEPENDENT(y)

       end subroutine



! ===================================================================

!            * Intutive Analysis:
!            ====================
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
!  please consider that a1, b2 indicate symbols and program point at which
!  they are defined. This helps us to keep track of recent definition
!  along various paths under examination.
!
! 1. a=x*5          x0->a1
! 2. b=a            x0->a1->b2
! 3. if(a>10)       x0->a1->b2
! 4. c=3            x0->a1->b2
! 5. c=a*(-1)       x0->a1->b2,     x0->a1->c5
! 6. y=3*b+c        x0->a1->b2->y6, x0->a1->c6->y6   [Active Path]
!
! ==================================================================
!
!            * ICFGActivity Analysis
!            =======================
!

!      subroutine head(x,y)
                             !  [u:  *y,b,c,a,*x] 
                             !  [v:  *x]
                             !  [iA: *x]
!
!         a = x * 5          Active Stmt  
!
                             !  [u:  *y,b,c,a]    
                             !  [v:  *x,a]
                             !  [iA: a]
!
!         b = a              Active Stmt
!  
                             !  [u:  *y,b,c,a] 
                             !  [v:  *x,a,b]
                             !  [iA: a,b]
!
!         if (a > 10) then   Active Stmt
!           
                             !  [u:  *y,b,c]
                             !  [v:  *x,a,b]
                             !  [iA: b]
!
!            c = 3           Active Stmt
!         
                             !  [u:  *y,b,c]   
                             !  [v:  *x,a,b]
                             !  [iA: b]
!         else
                             !  [u:  *y,b,c,a]  
                             !  [v:  *x,a,b]
                             !  [iA: a,b]
!
!            c = a * (-1)    Active Stmt
!           
                             !  [u:  *y,b,c]     
                             !  [v:  *x,a,b,c]
                             !  [iA: b,c]
!         end if 
                             !  [u:  *y,b,c]  
                             !  [v:  *x,a,b,c]
                             !  [iA: b,c]
!
!         y = 3 * b + c      Active Stmt
!
                             !  [u:  *y]  
                             !  [v:  *x,a,b,c,*y]
                             !  [iA: *y]
!       end subroutine

! ==============================================================
