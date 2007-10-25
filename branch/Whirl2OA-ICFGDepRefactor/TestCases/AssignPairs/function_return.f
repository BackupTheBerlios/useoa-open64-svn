
      PROGRAM ch2608
       IMPLICIT NONE
       INTEGER :: I,J,Result
       INTEGER :: GCD
         Result=GCD(I,J)
       END PROGRAM ch2608

       PURE INTEGER FUNCTION GCD(A,B)
        IMPLICIT NONE
        INTEGER , INTENT(IN) :: A,B
        GCD=A+B
       END FUNCTION GCD

