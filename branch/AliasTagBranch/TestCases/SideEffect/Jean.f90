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
      

! Problem: Wrong LMOD, LDEF, LUSE and LREF sets in the procedure foo.
! Problem: In the procedure head, MOD, DEF, REF, USE sets at the Call-site 
!          does not match.
! AliasTags blow up SideEffect Results.
      

         subroutine foo(x,y) 
           double precision, dimension(2) :: x
           double precision y
           y=x(1)*x(2)

!  MOD()  = (foo::*y, head::*y, head::py),   
!  LMOD() = (foo::*y, head::*y, head::py),
!  DEF()  = (foo::*y, head::*y, head::py),
!  LDEF() = (foo::*y, head::*y, head::py),
!  REF()  = (foo::*x(), foo::*x, head::*x, head::px, head::px()), 
!  LREF() = (foo::*x(), foo::*x, head::*x, head::px, head::px()),
!  USE()  = (foo::*x(), foo::*x, head::*x, head::px, head::px()),
!  LUSE() = (foo::*x(), foo::*x, head::*x, head::px, head::px())

         end subroutine

         subroutine head(x,y) 
           double precision, dimension(2) :: x, px
           double precision y, py
           px(1)=1.0

!  MOD()  = (foo::*x(), foo::*x, head::*x, head::px, head::px())
!  LMOD   = (foo::*x(), foo::*x, head::*x, head::px, head::px())           
!  DEF()  = (),
!  LDEF() = (),
!  REF()  = (),   
!  LREF() = (),
!  USE()  = (),
!  LUSE() = ()

           px(2)=2.0


!  MOD()  = (foo::*x(), foo::*x, head::*x, head::px, head::px())
!  LMOD   = (foo::*x(), foo::*x, head::*x, head::px, head::px())
!  DEF()  = (),
!  LDEF() = (),
!  REF()  = (),
!  LREF() = (),
!  USE()  = (),
!  LUSE() = ()


           call foo(x,y)

!  MOD()  = (foo::*x(), foo::*x, head::*x, head::px, head::px(), head::*y, bar::*y, head::py)
!  LMOD() = ()
!  DEF()  = (head::*y, bar::*y, head:py),              
!  LDEF() = ()
!  REF()  = (head::*x, foo:*x, foo::*x(), head::x, head::y, head::py), 
!  LREF() = ()
!  USE()  = (head::*x, foo:*x, foo::*x(), head::x, head::y, head::py),
!  LUSE() = ()


           call foo(px,py)

!  MOD()  = (foo::*x(), foo::*x, head::*x, head::px, head::px(), head::*y, bar::*y, head::py)
!  LMOD() = ()
!  DEF()  = (head::*y, bar::*y, head::py),
!  LDEF() = ()
!  REF()  = (head::*x, foo:*x, foo::*x(), head::x, head::y, head::py),
!  LREF() = ()
!  USE()  = (head::*x, foo:*x, foo::*x(), head::x, head::y, head::py),
!  LUSE() = ()


         end subroutine


