! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!                       SideEffect Analysis
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

       subroutine foo(m,t) 
         integer :: m,t
         t = m
       end subroutine

       subroutine head() 
         integer x
         integer p,q

         call foo(x,q)
         p = 5
         call foo(p,q)

       end subroutine


! =======================================
! Interprocedural SideEffect Results
! =======================================

! Procedure foo:
! 
! LMOD : *t
! MOD  : *t
! LDEF : 
! DEF  : 
! LUSE : *m
! USE  : *m
! LREF : *m
! REF  : *m


! Procedure head:
!
! LMOD : p
! MOD  : p,q
! LDEF : p
! DEF  : p
! LUSE : 
! USE  : p,x
! LREF : 
! REF  : p,x




