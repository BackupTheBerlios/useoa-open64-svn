! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:
!              [X] Active Path through Parameters at procedure call.
!                  [test of flow-sensitivity and context-insensitivity]
!
! Testing :
!              [X] ActiveStmts : All statements are active except (t3=f*30)
!              [X] ActiveMemRefExprs: x, t1, t2, *f, *a, *b
!              [X] ActiveSyms       : x, t1, t2, f, a, b        
!
! Status/Issues : [X] Everything is MayDefs and therefore not precise results.
!                 [X] Should Call Statements be Active ?
!                 [X] Do we need active MemRefHandles
!
! Note:    All Definitions are MayDefs
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                  * Example
!                  =========

       subroutine head(x, f)
         double precision :: x
         double precision :: f
         double precision t1, t2, t3

c$openad INDEPENDENT(x)
          t1=x*f   
          call bar(t1,t2)
          t3=f*30     
          f=t1+t2      
c$openad DEPENDENT(f)

       end subroutine




       subroutine bar(a,b)
         double precision a,b
          b = a   
         return
       end subroutine


! ===============================================================
!                  * Intutive Analysis
!                  ======================
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
!  please consider that x1, b4 indicate symbols and program point at which
!  they are defined. This helps us to keep track of recent definition
!  along various paths under examination.
!
!  1. t1=x*f       x0->t1,1
!  2. a=t1         x0->t1,1->a2 
!  3. b=t2         x0->t1,1->a2
!  4. b=a          x0->t1,1->a2->b4
!  5. t1=a         x0->t1,1->a2->b4
!  6. t2=b         x0->t1,1->a2->b4->t2,6
!  7. t3=f*30      x0->t1,1->a2->b4->t2,6
!  8. f=t1+t2      x0->t1,1->a2->b4->t2,6->f8   [Active Path]
! ===============================================================

!                  * ICFGActivity Analysis
!                  =======================

!     subroutine head(x, f) 
!
!                         [u: f,*f,t1,t2,*x]  [v: x,*x]           [iA: x]           !      
!       t1=x*f            [Stmt: Active]
!
!                         [u: t1,*f,t1,t2]    [v: x,*x,t1]        [iA: t1]
!
!       call bar(t1,t2)   [Stmt: Active]
!
!                         [u: f,*f,t1,t2]     [v: x,*x,t1,t2]     [iA: t1,t2]
!
!       t3=f*30           [Stmt: InActive]
!
!                         [u: f,*f,t1,t2]     [v: x,*x,t1,t2]     [iA: t1,t2]
!
!       f=t1+t2           [Stmt: Active]
!
!                         [u: f,*f]           [v: x,*x,t1,t2,*f]  [iA: *f]  
!
!     end subroutine
!
!
!     subroutine bar(a,b)
!
!                        [u: *a]    [v: *a]    [iA: *a]
!
!       b = a            [Stmt: Active]
!
!                        [u: *a,*b] [v: *a,*b] [iA: *a,*b]
!       return
!                        [u: *a,*b] [v: *a,*b] [iA: *a,*b]
!
!     end subroutine
!
! =====================================================================


