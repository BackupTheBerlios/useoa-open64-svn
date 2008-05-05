!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test -- Some variables are Used, some Defined, some both  !
!                                                           !
! x is both used and defined                                !
! a is only being defined                                   !
! b is only being used                                      !
! c is both used and defined                                !
! d is only being used                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! FIXME: 
! PLM May 3rd 2008
! Question: Should we get 'a' as ref in the procedure test ?


      
        program test
          integer :: x
          integer :: a, b, c, d
          x = 2
          call f(x,a,b,c,d);
        end program test

        subroutine f(n,o,p,q,r)
          integer :: n
          n = n + 1
          o = p + q + 1
          q = r + 1
        end subroutine


! ============================================
! Interprocedural SideEffect Results
! ============================================

! test()
! ======
! LMOD : x
! MOD  : x,a,c
! LDEF : x
! DEF  : x
! LUSE : 
! USE  : x,b,c,d
! LREF : x,b,c,d
! REF  : x,b,c,d

! f()
! =====
! LMOD : *n, *o, *q
! MOD  : *n, *o, *q
! LDEF :
! DEF  :
! LUSE : *n, *p, *q, *r
! USE  : *n, *p, *q, *r
! LREF : *n, *p, *q, *r
! REF  : *n, *p, *q, *r

