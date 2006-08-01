! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1007.f90
! In the Program ch1007 ,
! For the first outermost Do loop: Degree = {180, 165, 15}
! For the Do loop Nested Inside First Outermost Do loop: Time = 0, Strip = {0, 14}
! For the second outermost Do loop: Degree = {-180, 180}

PROGRAM ch1007
IMPLICIT NONE
REAL , DIMENSION(-180:180) :: Time=0
INTEGER :: Degree,Strip
REAL :: Value

  !Reaching Constants: Degree = TOP
  DO Degree=-180,165,15 
  
  !Reaching Constants: Degree = BOTTOM, Value = TOP
  Value=Degree/15

    !Reaching Constants Strip = TOP
    DO Strip=0,14

      !Reaching Constants: Degree = BOTTOM, Strip = BOTTOM, Time(*) = 0
      Time(Degree+Strip)=Value 
    ENDDO
  ENDDO

  !Reaching Constants: Degree = BOTTOM
  DO Degree=-180,180
    
    !Reaching Constants: Degree = BOTTOM, Time(*) = TOP
    PRINT *,Degree,' ',Time(Degree)
  END DO
END PROGRAM ch1007  
