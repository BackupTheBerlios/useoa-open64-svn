
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:
!            [X] Jaewook's paper on Context-Sensitive Activity Analysis
!                - Multiple Function Calls using same actual parameters
!                  and order in which actual parameters are used/defined
!                  inside callee will affect Activity analysis.
!
! Testing :
!            [X] ActiveStmts: All Statements are Active
!            [X] ActiveMemRefExprs: x,a,y,*f,*b,*g
!            [X] ActiveSyms:  x,a,y,f,b,g
!
! Note:    All Definitions are MayDefs
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!              Example:
!              =======
!

      subroutine foo()
          double precision ::x,a,y
c$openad INDEPENDENT(x)
           call bar(x,a,y) 
           call bar(x,a,y)
c$openad DEPENDENT(y)
      end subroutine

      subroutine bar(f,b,g)
          double precision :: g,b,f
          g=b
          b=f
      end subroutine


! ==========================================================================
!
!              Intutive Analysis
!              ==================
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
!
! PP Stmt   Paths
!
! 1. f=x     x1->f1
!
! 2. b=a     x1->f1
!
! 3. y=g     x1->f1
!
! 4. g=b     x1->f1
!
! 5. b=f     x1->f1->b5,
!
! 6. x=f     x1->f1->b5,    
!            x1->f1->x6
!
! 7. a=b     x1->f1->b5->a7,     
!            x1->f1->x6
!
! 8. y=g     x1->f1->b5->a7,   
!            x1->f1->x6
!
! 9. f=x     x1->f1->b5->a7,   
!            x1->f1->x6->f9
!
!10. b=a     x1->f1->b5->a7->b10, 
!            x1->f1->x6->f9
!
!11. g=y     x1->f1->b5->a7->b10, 
!            x1->f1->x6->f9
!
!12. g=b     x1->f1->b5->a7->b10->g12,   
!            x1->f1->x6->f9
!
!13. b=f     x1->f1->b5->a7->b10->g12,   
!            x1->f1->x6->f9->b13
!
!14. x=f     x1->f1->b5->a7->b10->g12, 
!            x1->f1->x6->f9->b13, 
!            x1->f1->x6->f9->x14
!
!15. a=b     x1->f1->b5->a7->b10->g12,   
!            x1->f1->x6->f9->b13->a15, 
!            x1->f1->x6->f9->x14
!
!16. y=g     x1->f1->b5->a7->b10->g12, 
!            x->f->x->f->b->a,  
!            x->f->x->f->x    
!            x1->f1->b5->a7->b10->g12->y16     [Active Path]
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                   ICFGActivity Analysis
!                   ======================


!      subroutine foo()
!          double precision ::x,a,y
!
!                           [u: y,a,x]   [v: x]        [iA: x]
!
!          call bar(x,a,y)  [Active Stmt]
!
!                           [u: y,a]     [v: a,x]      [iA: a]
! 
!          call bar(x,a,y)  [Active Stmt]
!
!                           [u: y]       [v: a,x,y]    [iA: y]
!     end subroutine
!
!
!     subroutine bar(f,b,g)
!                          [u: *g,*b,*f]  [v: *f,*b]      [iA: *f,*b]
!         g=b
!                          [u: *g,*b,*f]  [v: *f,*b,*g]   [iA: *f,*b,*g]
!         b=f
!                          [u: *g,*b]     [v: *f,*b,*g]   [iA: *g,*b]
!
!     end subroutine
!
! ===================================================================== 
     
