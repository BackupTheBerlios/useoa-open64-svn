PROGRAM ch2303
IMPLICIT NONE
REAL,DIMENSION(1:100)::A,B
INTEGER :: Nos,I
CHARACTER(LEN=20)::Filename
INTERFACE
  SUBROUTINE Readin(Name,X,Y,N)
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: N
    REAL,DIMENSION(1:N),INTENT(OUT)::X,Y
    CHARACTER (LEN=*),INTENT(IN)::Name
  END SUBROUTINE Readin
END INTERFACE
  PRINT *,' Type in the name of the data file'
  READ '(A)' , Filename
  PRINT *,' Input the number of items in the file'
  READ * , Nos
  CALL Readin(Filename,A,B,Nos)
  PRINT * , ' Data read in was'
  DO I=1,Nos
    PRINT *,' ',A(I),' ',B(I)
  ENDDO
END PROGRAM ch2303
SUBROUTINE Readin(Name,X,Y,N)
IMPLICIT NONE
INTEGER , INTENT(IN) :: N
REAL,DIMENSION(1:N),INTENT(OUT)::X,Y
CHARACTER (LEN=*),INTENT(IN)::Name
INTEGER::I
  OPEN(UNIT=10,STATUS='OLD',FILE=Name)
  DO I=1,N
    READ(10,*)X(I),Y(I)
  END DO   
  CLOSE(UNIT=10) 
END SUBROUTINE Readin


