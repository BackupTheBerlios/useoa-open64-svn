PROGRAM ch2608
IMPLICIT NONE
INTEGER :: I,J,Result
INTEGER :: GCD
  PRINT *,' Type in two integers'
  READ *,I,J
  Result=GCD(I,J)
  PRINT *,' GCD is ',Result
END PROGRAM ch2608

PURE INTEGER FUNCTION GCD(A,B)
IMPLICIT NONE
INTEGER , INTENT(IN) :: A,B
INTEGER :: Temp  
  IF (A < B) THEN
    Temp=A
  ELSE
    Temp=B
  ENDIF
  DO WHILE ((MOD(A,Temp) /= 0) .OR. (MOD(B,Temp) /=0))
    Temp=Temp-1
  END DO
  GCD=Temp
END FUNCTION GCD
