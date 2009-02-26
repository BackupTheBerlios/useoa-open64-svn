! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch0802.f90


PROGRAM ch0802
IMPLICIT NONE
REAL :: A,B,C
INTEGER :: I

  !Reaching Constants: A = 1.5
  A = 1.5

  !Reaching Constants B = 2.0
  B = 2.0

  !Reaching Constants: A = 1.5, B = 2.0
  C = A / B

  !Reaching Constants: A = 1.5, B = 2.0
  I = A / B
  PRINT *,A,B
  PRINT *,C
  PRINT *,I
END PROGRAM ch0802
