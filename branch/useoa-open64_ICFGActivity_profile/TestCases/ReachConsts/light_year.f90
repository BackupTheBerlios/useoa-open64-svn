! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch0804.f90
        


PROGRAM ch0804
IMPLICIT NONE
REAL :: Light_Minute, Distance, Elapse
INTEGER :: Minute, Second
REAL , PARAMETER :: Light_Year=9.46*10**12
! Light_year  : Distance travelled by light
! in one year in km
! Light_minute : Distance travelled by light
! in one minute in km
! Distance : Distance from sun to earth in km
! Elapse : Time taken to travel a
! distance (Distance) in minutes
! Minute : Integer number part of elapse
! Second : Integer number of seconds
! equivalent to fractional part of elapse
!

  !Reaching Constants: Light_Year = 1135.2, Light_minute = TOP
  Light_minute = Light_Year/(365.25 * 24.0 * 60.0)

  !Reaching Constants: Distance = TOP
  Distance = 150.0 * 10 ** 6

  !Reaching Constants: Elapse = TOP, Light_minute = TOP
  Elapse = Distance / Light_minute

  !Reaching Constants: Elapse = TOP, Minute = TOP
  Minute = Elapse

  !Reaching Constants: Elaspe = TOP, Minute = TOP, Second = TOP
  Second = (Elapse - Minute) * 60
  Print *,' Light takes ' , Minute,' Minutes'
  Print *,'           ' , Second,' Seconds'
  Print *,' To reach the earth from the sun'
END PROGRAM ch0804
