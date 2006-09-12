! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1005.f90



PROGRAM ch1005
IMPLICIT NONE
INTEGER  , PARAMETER :: NR=5
INTEGER  , PARAMETER :: NC=10
INTEGER  , PARAMETER :: NF=3
INTEGER :: Row,Column,Floor
CHARACTER*1 , DIMENSION(1:NR,1:NC,1:NF) :: Seats=' '

  !Reaching Defintion: Floor=TOP, NF = 3
  DO  Floor=1,NF
   
    !Reaching Definition: ROW = TOP, NR = 5
    DO  Row=1,NR

      !Reaching Definition: Row = BOTTOM, Column = BOTTOM, NC = 10, Floor = BOTTOM
      READ *,(Seats(Row,Column,Floor),Column=1,NC)
    ENDDO
  ENDDO
  PRINT *,' Seat plan is'

  !Reaching Definition: Floor = BOTTOM, NF = 3
  DO  Floor=1,NF

    !Reaching Definition Row = TOP, NR = 5 
    DO  Row=1,NR

      !Reaching Definition Row = BOTTOM, Column = BOTTOM, NC = 10
      PRINT *,(Seats(Row,Column,Floor),Column=1,NC)
    ENDDO
  ENDDO
END PROGRAM ch1005
