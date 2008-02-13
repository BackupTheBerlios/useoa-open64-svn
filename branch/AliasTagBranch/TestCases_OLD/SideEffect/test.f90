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
