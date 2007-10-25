  SUBROUTINE sub1(a,b,c)
   IMPLICIT NONE
   EXTERNAL sum_sq  ! Should declare or use an INTERFACE
   REAL :: a, b, c, s
     CALL sum_sq(a,b,c,s)
   END SUBROUTINE sub1

  SUBROUTINE sum_sq(aa,bb,cc,ss)
   IMPLICIT NONE
   REAL, INTENT(IN) :: aa, bb, cc
   REAL, INTENT(OUT) :: ss
    ss = aa*aa + bb*bb + cc*cc
  END SUBROUTINE sum_sq
