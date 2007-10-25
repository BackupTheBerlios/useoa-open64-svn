PROGRAM ch2604
IMPLICIT NONE

INTERFACE
  FUNCTION Running_Average(R,How_Many)
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: How_Many
    REAL , DIMENSION (:) , INTENT(IN) :: R
    REAL , DIMENSION(how_many) :: Running_Average
  END FUNCTION Running_Average
END INTERFACE

INTERFACE Read_Data

  SUBROUTINE RR(File_Name,Raw_Data,How_Many)
    IMPLICIT NONE
    CHARACTER (LEN=*) , INTENT(IN) :: File_Name
    INTEGER , INTENT(IN) :: How_Many
    REAL , INTENT(OUT) , &
    DIMENSION(:) :: Raw_Data
  END SUBROUTINE RR

  SUBROUTINE RI(File_Name,Raw_Data,How_Many)
    IMPLICIT NONE
    CHARACTER (LEN=*) , INTENT(IN) :: File_Name
    INTEGER , INTENT(IN) :: How_Many
    INTEGER , INTENT(OUT) , &
    DIMENSION(:) :: Raw_Data
  END SUBROUTINE RI

END INTERFACE

INTEGER :: How_Many
CHARACTER (LEN=20) :: File_Name
REAL , ALLOCATABLE , DIMENSION(:)  :: Raw_Data
REAL , ALLOCATABLE , DIMENSION(:)  :: RA
INTEGER :: I
  PRINT * , ' How many data items are there?'
  READ  * , How_Many
  PRINT * , ' What is the file name?'
  READ '(A)',File_Name 
  ALLOCATE(Raw_Data(how_many))
  ALLOCATE(RA(how_many))
  CALL Read_Data(File_Name,Raw_Data,How_Many)
  RA=Running_Average(Raw_Data,How_Many)
  DO I=1,How_Many
    PRINT *,Raw_Data(I),'   ' ,RA(I)
  END DO
END PROGRAM ch2604

FUNCTION Running_Average(R,How_Many)
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(IN) , DIMENSION(:) :: R
REAL , DIMENSION(how_many) :: Running_Average
INTEGER :: I
REAL :: Sum=0.0
  DO I=1,How_Many
    Sum = Sum + R(I)
    Running_Average(I)=Sum/I
  END DO
END FUNCTION Running_Average

SUBROUTINE RR(File_Name,Raw_Data,How_Many)
IMPLICIT NONE
CHARACTER (LEN=*) , INTENT(IN) :: File_Name
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(OUT) , &
  DIMENSION(:) :: Raw_Data
INTEGER :: I
  OPEN(FILE=File_Name,UNIT=1)
  DO I=1,How_Many
    READ (UNIT=1,FMT=*) Raw_Data(I)
  ENDDO
END SUBROUTINE RR

SUBROUTINE RI(File_Name,Raw_Data,How_Many)
IMPLICIT NONE
CHARACTER (LEN=*) , INTENT(IN):: File_Name
INTEGER , INTENT(IN) :: How_Many
INTEGER , INTENT(OUT) , &
  DIMENSION(:) :: Raw_Data
INTEGER :: I
  OPEN(FILE=File_Name,UNIT=1)
  DO I=1,How_Many
    READ (UNIT=1,FMT=*) Raw_Data(I)
  ENDDO
END SUBROUTINE RI


