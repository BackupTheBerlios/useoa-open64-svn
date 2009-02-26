MODULE T_Position
IMPLICIT NONE
TYPE Position
  INTEGER :: X
  INTEGER :: Y
  INTEGER :: Z
END TYPE Position

INTERFACE OPERATOR (+)
  MODULE PROCEDURE New_Position
END INTERFACE

CONTAINS

FUNCTION New_Position(A,B)
TYPE (Position) ,INTENT(IN) :: A,B
TYPE (Position) :: New_Position
  New_Position % X = A % X + B % X
  New_Position % Y = A % Y + B % Y
  New_Position % Z = A % Z + B % Z
END FUNCTION New_Position

END MODULE T_Position

PROGRAM ch2605
USE T_Position
IMPLICIT NONE
TYPE (Position) :: A,B,C
  A%X=10
  A%Y=10
  A%Z=10
  B%X=20
  B%Y=20
  B%Z=20
  C=A+B
  PRINT *,A
  PRINT *,B
  PRINT *,C
END PROGRAM ch2605
