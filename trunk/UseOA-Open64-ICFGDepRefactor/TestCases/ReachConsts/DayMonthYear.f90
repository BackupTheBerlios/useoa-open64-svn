! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch0702.f90



PROGRAM ch1602
IMPLICIT NONE
INTEGER :: Year , N , Month , Day , T
!
! calculates day and month from year and
! day-within-year
! t is an offset to account for leap years.
! Note that the first criteria is division by 4
! but that centuries are only
! leap years if divisible by 400
! not 100 (4 * 25) alone.
!
  PRINT*,' year, followed by day within year'

  !Reaching Constants: Year=TOP, N=TOP
  READ*,Year,N
!   checking for leap years 

  !Reaching Constants: Year = TOP
  IF ((Year/4)*4 == Year ) THEN

    !Reaching Constants: T = TOP  
    T=1

    !Reaching Constants: Year = TOP
    IF ((Year/400)*400 == Year ) THEN

      !Reaching Constants: T = 1  
      T=1
      
      !Reaching Constants: Year = TOP 
    ELSEIF ((Year/100)*100 == Year) THEN

      !Reaching Constants: T = 1
      T=0
    ENDIF  
  ELSE
    
    !Reaching Constants: T = TOP  
    T=0
  ENDIF
!    accounting for February 
  IF(N > (59+T))THEN
   
    !Reaching Definition: T = Bottom  
    Day=N+2-T 
  ELSE
    Day=N
  ENDIF
  Month=(Day+91)*100/3055
  Day=(Day+91)-(Month*3055)/100 
  Month=Month-2
  PRINT*,' CALENDAR DATE IS ', Day , Month , Year
END PROGRAM ch1602
