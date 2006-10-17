! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1008.f90


PROGRAM ch1008
IMPLICIT NONE
!
! 1 us gallon = 3.7854118 litres
! 1 uk gallon = 4.545     litres
!
INTEGER :: Litre
REAL    :: Gallon,USGallon
  DO Litre = 1,10

    !Reaching Constants: Litre = (1,10)
    Gallon   = Litre * 0.2641925
    USGallon = Litre * 0.220022
    PRINT *,Litre, ' ',Gallon,' ',USGallon
  END DO
END PROGRAM ch1008
