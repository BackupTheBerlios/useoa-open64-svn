! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch0702.f90

!Problems TestResults/ReachConsts:  Missing Information about N = 3 


PROGRAM ch0702
!
! This program reads in three numbers and sums 
! and averages them
!
IMPLICIT NONE
!Reaching Constants: N1 = N2 = N3 = Average = Total = TOP
REAL :: N1,N2,N3,Average = 0.0, Total = 0.0
INTEGER :: N = 3
  PRINT *,' Type in three numbers.'
  PRINT *,' Separated by spaces or commas'

  !Reaching Constants N1 = N2 = N3 = TOP
  READ *,N1,N2,N3

  ! Reaching Constansts: N1=TOP, N2=TOP, N3=TOP, Total = 0.0
  Total= N1 + N2 + N3

  ! Reaching Constatnts, Total = Top, Average = 0.0, N = 3;
  Average=Total/N
  
  !Reaching Constants: Total = TOP
  PRINT *,'Total of numbers is ',Total

  !Reaching Constants: Average = TOP
  PRINT *,'Average of the numbers is ',Average
END PROGRAM ch0702
