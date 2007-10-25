! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch0702.f90


PROGRAM ch1105
IMPLICIT NONE
integer , parameter :: n=12
REAL :: Total=0.0, Average=0.0
REAL , DIMENSION(1:n) :: RainFall = &
  (/3.1,2.0,2.4,2.1,2.2,2.2,1.8,2.2,2.7,2.9,3.1,3.1/)
INTEGER :: Month
  DO Month=1,n

    !Reaching Constant Total = 0.0,  Month = 1
    Total = Total + RainFall(Month)
  ENDDO

  !Reaching Definition Average = 0.0, n = 12
  Average = Total / n
  PRINT *,' Average monthly rainfall was'
  PRINT *, Average
END PROGRAM ch1105
