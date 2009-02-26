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
!            [X] ActiveMemRefExprs: x,y,*f,*b,*g
!            [X] ActiveSyms:  x,y,f,b,g
!
! Note:    All Definitions are MayDefs
!
! Issues: [X] No Active Variables in the output of ICFGActivity
!             Analysis. 
!             
!         [X] Intutive Flow-Sensitive ICFGActivity analysis overestimates that
!             variable "foo::a" is active. But intutive analysis shows
!             that there is no value flow path between Independent variable
!             "foo::x" and Dependent variable "foo::y" through variable 
!             "foo::a"
! 
!             I think the, the main reason behind this problem is
!             context-insensitivity and also Unification-based alias
!             analysis algorithm.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                    ! Example:
                    ! ========

      subroutine foo
         double precision :: x,a,y

c$openad INDEPENDENT(x)
         call bar(x,a,y)
         call bar(x,a,y)
c$openad DEPENDENT(y)
      end subroutine

      subroutine bar(f,b,g)
         double precision :: b,f,g

         b=f
         g=b
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
! 1. f=x    x1->f1
!
! 2. b=a    x1->f1
!
! 3. g=y    x1->f1
!
! 4. b=f    x1->f1->b4
!
! 5. g=b    x1->f1->b4->g5
!
! 6. x=f    x1->f1->b4->g5,        
!           x1->f1->x6, 
!
! 7. a=b    x1->f1->b4->g5, 
!           x1->f1->x6,
!
! 8. y=g    x1->f1->b1->g1->y8,           => [active path]
!           x1->f1->x6,
!
! 9. f=x    x1->f1->b1->g1->y8,           => [active path]
!           x1->f1->x6->f9,
!
!10. b=a    x1->f1->b1->g1->y8,           => [active path]  
!           x1->f1->x6->f9,
!
!11. g=y    x1->f1->b1->g1->y8->g11,      
!           x1->f1->x6->f9,
!
!12. b=f    x1->f1->b1->g1->y8->g11,
!           x1->f1->x6->f9->b12,
!
!13. g=b    x1->f1->b1->g1->y8->g11, 
!           x1->f1->x6->f9->b12->g13,
!
!14. x=f    x1->f1->b1->g1->y8->g11, 
!           x1->f1->x6->f9->b12->g13, 
!           x1->f1->x6->f9->x14
!
!15. a=b    x1->f1->b1->g1->y8->g11, 
!           x1->f1->x6->f9->b12->g13, 
!           x1->f1->x6->f9->x14, 
!           x1->f1->x6->f9->b12->a15
!
!16. y=g    
!           x1->f1->b1->g1->y8->g11,
!           x1->f1->x6->f9->b12->g13->y16,   =>   [active path]
!           x1->f1->x6->f9->x14,
!           x1->f1->x6->f9->b12->a15
!
! ================================================================

      ! Flow-sensitive ICFGActivity Analysis
      ! =====================================

!      subroutine foo
!                          [u: x,a,y]  [v: x]      [iA: a]
!         call bar(x,a,y)
!         
!                          [u: x,a,y]  [v: x,a,y]  [iA: x,a,y]   
!         call bar(x,a,y) 
!                          [u: y]      [v: x,a,y]  [iA: y]
!      end subroutine
!
!
!      subroutine bar(f,b,g)
!                          [u: *g,*b,*f] [v: *f,*b,*g] [iA: *f,*b,*g]
!         b=f
!                          [u: *g,*b,*f] [v: *f,*b,*g] [iA: *f,*b,*g]
!         g=b
!                          [u: *g,*b,*f] [v: *f,*b,*g] [iA: *f,*b,*g]
!
!      end subroutine
!
! =========================================================================
