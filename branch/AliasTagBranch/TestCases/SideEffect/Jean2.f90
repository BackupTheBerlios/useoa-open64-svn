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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! displays possible MOD. LMOD imprecision for x
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem: No results for the callsites ????

       subroutine foo(m,t) 
         integer :: m,t
         t = m

!    MOD()  = (foo::*t, head::q),     LMOD() = (foo::*t, head::q)
!    DEF()  = ( ),                    LDEF() = ( )         
!    REF()  = (foo::*t, head::q),     LREF() = (foo::*m, head::x, head::p)
!    USE()  = (foo::*t, head::q),     LUSE() = (foo::*m, head::x, head::p)
         
       end subroutine

       subroutine head() 
         integer x
         integer p,q
         call foo(x,q)

!    MOD()  = (head::q, foo::*t),              LMOD() = ()
!    DEF()  = (head::q, foo::*t),              LDEF() = ()
!    REF()  = (head::x, head::p, foo::*m),     LREF() = ()
!    USE()  = (head::x, head::p, foo::*m),     LUSE() = ()
         
         p = 5

!    MOD()  = (head::p, head::x, foo::*m),      LMOD() = (head::p, head::x, foo::*m)
!    DEF()  = (head::p, head::x, foo::*m),      LDEF() = (head::p, head::x, foo::*m)
!    REF()  = (),                               LREF() = ()
!    USE()  = (),                               LUSE() = ()
         
         call foo(p,q)

!    MOD()  = (head::q, foo::*t),              LMOD() = ()
!    DEF()  = (head::q, foo::*t),              LDEF() = ()
!    REF()  = (head::x, head::p, foo::*m),     LREF() = ()
!    USE()  = (head::x, head::p, foo::*m),     LUSE() = ()


       end subroutine


