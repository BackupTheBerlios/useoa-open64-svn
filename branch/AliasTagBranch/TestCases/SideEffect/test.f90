!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test -- Some variables are Used, some Defined, some both  !
!                                                           !
! x is both used and defined                                !
! a is only being defined                                   !
! b is only being used                                      !
! c is both used and defined                                !
! d is only being used                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem:  test::LMOD() = (*n) ???? Imprecise
      
        program test
          integer :: x
          integer :: a, b, c, d
          x = 2


!          MOD() = (x,*n),       LMOD() = (x,*n)
!          DEF() = (x,*n),       LDEF() = (x,*n) 
!          REF() = (),           LREF() = ()
!          USE() = (),           LUSE() = ()
          
          
          call f(x,a,b,c,d);

! MOD()  = (test::x, test::a, test::c, f::*n, f::*o, f::*q),    LMOD() = ()
! DEF()  = (test::x, test::a, test::c, f::*n, f::*o, f::*q),    LDEF() = ()
! REF()  = (test::a, test::b, test::c, test::c, test::d, f::n, f::p, f::q, f::r), 
! LREF() = ()
! USE()  = (test::a, test::b, test::c, test::c, test::d, f::n, f::p, f::q, f::r), 
! LUSE() = ()

        end program test

        subroutine f(n,o,p,q,r)
          integer :: n
          n = n + 1

!         MOD() = (f::*n),   LMOD() = (f::*n)
!         DEF() = (f::*n),   LDEF() = (f::*n)        
!         REF() = (f::*n),   LREF() = (f::*n)
!         USE() = (f::*n),   LUSE() = (f::*n)


          o = p + q + 1

!         MOD() = (f::*o),            LMOD() = (f::*o)
!         DEF() = (f::*o),            LDEF() = (f::*o) 
!         REF() = (f::*p,f::*q),      LREF() = (f::*p,f::*q)
!         USE() = (f::*p,f::*q),      LUSE() = (f::*p,f::*q)

          q = r + 1

!         MOD() = (f::*q),       LMOD() = (f::*q)
!         DEF() = (f::*q),       LMOD() = (f::*q)
!         REF() = (f::*r),       LREF() = (f::*r)
!         USE() = (f::*r),       LUSE() = (f::*r)


        end subroutine
