! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:
!          [X] arrays
!          [X] Conditional assignments [test of Flow-Sensitivity]
!
! Testing :
!          [X] ActiveStmts           :  All Statements are active
!          [X] ActiveTags/MemRefExprs:  x(), y()
!          [X] ActiveSyms            :  x,y
!
! Status/Issues: Do we need Active MemRefHandles?
!
! Note:    All Definitions are MayDefs
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                   * Example
!                   =========

       subroutine head()
         double precision x(2)
         double precision y(2)
         integer i
c$openad INDEPENDENT(x)
         i=1 
         do while (i<3)
           if (i<2) then  
             y(2)=x(1) 
           else
             y(1)=x(2)
           end if
           i=i+1    
         end do
         y(2)=y(1)*y(2) 
c$openad DEPENDENT(y)
       end subroutine


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
!  please consider that [x(),0], [y(),4] indicate symbols and program 
!  point at which they are defined. This helps us to keep track of 
!  recent definition along various paths under examination.
!
! 1. i=1               
! 2. do while (i<3)
! 3. if (i<2) then
! 4. y(2)=x(1)           x(),0 => y(),4            [Active Path]
! 5. y(1)=x(2)           x(),0 => y(),5            [Active Path]
! 6. i=i+1                 
! 7. y(2)=y(1)*y(2)      x(),0 => y(),4 => y(),7   [Active Path]
!                        x(),0 => y(),5 => y(),7   [Active Path]


! =====================================================================
!
!                 * ICFGActivity Analysis
!                 =======================
!

c      subroutine head()
c                         [u: y,y(),x()] [v: x,x()]         [iA: x()]
c
c         i=1             [Stmt: Active]
c
c                         [u: y,y(),x()] [v: x,x()]         [iA: x()]
c
c         do while (i<3)  [Stmt: Active]
c
c                         [u: y,y(),x()] [v: x,x()]         [iA: x()]
c
c          if (i<2) then  [Stmt: Active]
c
c                         [u: y,y(),x()] [v: x,x()]         [iA: x()]
c
c            y(2)=x(1)    [Stmt: Active]
c
c                         [u: y,y(),x()] [v: x,x(),y()]     [iA: y()]
c          else
c                         [u: y,y(),x()] [v: x,x()]         [iA: x()]
c
c            y(1)=x(2)    [Stmt: Active]
c
c                         [u: y,y()]     [v: x,x(),y()]     [iA: y()]
c          end if 
c                         [u: y,y()]     [v: x,x(),y()]     [iA: y()]
c
c          i=i+1          [Stmt: Active]
c
c                         [u: y,y()]     [v: x,x(),y()]     [iA: y()]
c        end do
c                         [u: y,y()]     [v: x,x(),y()]     [iA: y()]
c
c        y(2)=y(1)*y(2)   [Stmt: Active]
c
c                         [u: y,y()]     [v: x,x(),y()]     [iA: y()]
c      end subroutine


! =========================================================================

