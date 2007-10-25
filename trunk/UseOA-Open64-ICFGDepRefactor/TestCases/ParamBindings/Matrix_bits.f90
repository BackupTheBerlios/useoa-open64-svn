PROGRAM ch2304
IMPLICIT NONE
INTEGER, PARAMETER :: Max=10
REAL  , DIMENSION (1:Max,1:Max)::One,Two,Three,One_T
INTEGER :: I,N
INTERFACE
  SUBROUTINE Matrix_bits(A,B,C,A_T,N,Max)
  IMPLICIT NONE
  INTEGER, INTENT(IN):: N, Max
  REAL, DIMENSION (1:Max,1:Max), INTENT(IN) :: A,B
  REAL, DIMENSION (1:Max,1:Max), INTENT(OUT) :: C,A_T
  END SUBROUTINE Matrix_bits
END INTERFACE
    
    PRINT *,'Input size of matrices'
    READ*,N
    DO WHILE(N > Max)
      PRINT*,'size of matrices must be <= ',Max
      PRINT *,'Input size of matrices'
    READ*,N
    END DO
    DO I=1,N
      PRINT*, 'Input row ', I,' of One'
      READ*,One(I,1:N)
    END DO
    DO I=1,N
      PRINT*, 'Input row ', I,' of Two'
      READ*,Two(I,1:N)
    END DO
    CALL Matrix_bits(One,Two,Three,One_T,N,Max)
    PRINT*,' Matrix Three:'
    DO I=1,N
      PRINT *,Three(I,1:N)
    END DO
    PRINT *,' Matrix One_T:'
    DO I=1,N
      PRINT *,One_T(I,1:N)
    END DO
END PROGRAM ch2304

SUBROUTINE Matrix_bits(A,B,C,A_T,N,Max)
IMPLICIT NONE
INTEGER, INTENT(IN):: N, Max
REAL, DIMENSION (1:Max,1:Max), INTENT(IN) :: A,B
REAL, DIMENSION (1:Max,1:Max), INTENT(OUT) :: C,A_T

INTEGER::I,J,K
REAL:: Temp
!
! matrix multiplication C=A B
!
   DO I=1,N
      DO J=1,N
         Temp=0.0
         DO K=1,N
            Temp = Temp + A(I,K) * B (K,J)
         END DO
            C(I,J) = Temp
      END DO
   END DO
!
! set A_T to be transpose matrix A
  DO I=1,N
     DO J=1,N
        A_T(I,J) = A(J,I)
     END DO
 END DO
END SUBROUTINE Matrix_bits
