! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   SideEffect Analysis
!
! Definitions of different abstractions used in SideEffect Analysis
! Reference: Optimizing Compilers and Modern Architectures by Kennedy
!
! MOD(s)     = May be modified as a side effect of the call site 's'
!
! REF(s)     = May be referenced as a side effect at call site 's'
!
! USE(s)     = upward exposed use
! 
! DEF(s)     = Must defined on every path through the procedure p
!
! LMOD(stmt) = May be modified at statement s locally
!              
! LDEF(stmt) = Must Defined variables at statement s locally.
!              Always MustDef.
!
! LUSE(stmt) = uses in the procedure.
!              e.g. &x => x is not in the LUse set
! 
! LREF(stmt) = reference at statement s locally.     
!              e.g. &x => x is in the LREF set.
!
! Note: You will not get SideEffect results for MREs that are not
!       referred in the procedure. e.g. In the example below
!       px() is used but *x() is not used in the procedure head.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      

         subroutine foo(a,b) 
           double precision, dimension(2) :: a
           double precision :: b

           b=a(1)*a(2)

         end subroutine

         subroutine head(x,y) 
           double precision, dimension(2) :: x, px
           double precision y, py

           px(1)=1.0
           px(2)=2.0
           call foo(x,y)
           call foo(px,py)

         end subroutine



! =====================================================
! Interprocedural SideEffect Analysis
! =====================================================
!
! Procedure foo
! 
! LMOD = *b
! MOD  = *b
! LDEF =
! DEF  =
! LUSE = *a, *a()
! USE  = *a, *a()
! LREF = *a, *a()
! REF  = *a, *a()
!
!
! Procedure head
!
! LMOD = px(), px, *x
! MOD  = px(), px *x, *y, py
! LDEF = 
! DEF  =
! LUSE = x,y 
! USE  = x,y,px,*x,px()
! LREF = x,y
! REF  = x,y,px,*x,px()




