! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1002.f90


PROGRAM C1002
! Variables used 
! Height - used to hold the heights above sea level
! Long - used to represent the longitude
! Lat - used to represent the latitude
!    both restricted to integer values.
! Correct - holds the correction factor
IMPLICIT NONE
INTEGER , PARAMETER :: Size = 3
INTEGER  :: Lat , Long
REAL , DIMENSION(1:Size,1:Size) :: Height
REAL , PARAMETER :: Correct = 10.0

  !Reaching Constants: Size = 3, Lat = TOP
  DO Lat = 1,Size

    !Reaching Constants: Size = 3, Long = TOP
    DO Long = 1,Size

      !Reaching Constants: Lat = BOTTOM, Long = BOTTOM
      PRINT *,' Type in value at ',Lat,' ',Long

      !Reaching Constants: Lat = BOTTOM, Long = BOTTOM, Height(*,*) = TOP      
      READ * , Height(Lat,Long)
    ENDDO
  ENDDO

  !Reaching Constants: Size = 3, Lat = BOTTOM
  DO Lat = 1,Size

    !Reaching Constants: Size = 3, Long = BOTTOM
    DO Long = 1,Size
     
      !Reaching Constants: Correct = 10.0, Height(Lat,Long) = TOP, Lat = BOTTOM, Long = BOTTOM                
      Height(Lat,Long) = Height(Lat,Long) + Correct
    ENDDO
  ENDDO
  PRINT * , ' Corrected data is '

  !Reaching Constants: Size = 3, Lat = BOTTOM
  DO Lat = 1,Size
    
    !Reaching Constants: Size = 3, Long = BOTTOM
    DO Long = 1,Size

      !Reaching Constants: Height(*,*) = TOP
      PRINT * , Height(Lat,Long)
    ENDDO
  ENDDO
END PROGRAM C1002
