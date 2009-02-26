!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test -- Some variables are Used, some Defined, some both  !
!                                                           !
! x is both used and defined                                !
! a is only being defined                                   !
! b is only being used                                      !
! c is both used and defined                                !
! d is only being used                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      
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
! MOD  : a,c,x
! LDEF : x
! DEF  : x
! LUSE : 
! USE  : b,c,d,x
! LREF : x
! REF  : a,b,c,d,x



! f()
! =====
! LMOD : *o, *q, *n
! MOD  : *o, *q, *n
! LDEF :
! DEF  :
! LUSE : *p, *q, *r, *n
! USE  : *p, *q, *r, *n
! LREF : *o, *p, *q, *r, *n
! REF  : *o, *p, *q, *r, *n


