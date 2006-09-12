! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1101.f90
! For the Program ch1101,
! RainFall_ins = 0.0,  RainFall_cms = 0.0


PROGRAM ch1101
IMPLICIT NONE
INTEGER , PARAMETER :: N=12
REAL , DIMENSION(1:N) :: RainFall_ins=0.0
REAL , DIMENSION(1:N) :: RainFall_cms=0.0
INTEGER :: Month
  PRINT *, ' Input the rainfall values in inches'
  READ *, RainFall_ins

  !Reaching Constants: RainFall_cms = 0.0, RainFall_ins = TOP
  RainFall_cms=RainFall_ins * 2.54

  !Reaching Constants: N = 12, Month = TOP
  DO Month=1,N

    !Reaching Constants: Month = BOTTOM, RainFall_ins(*) = TOP, RainFall_cms(*)= TOP
    PRINT * , ' ', Month , ' ' , &
                 RainFall_ins(Month) , ' ' , &
                 RainFall_cms(Month)
  END DO
END PROGRAM ch1101
