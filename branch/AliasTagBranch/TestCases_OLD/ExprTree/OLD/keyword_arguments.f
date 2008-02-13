
       IMPLICIT NONE 
       INTERFACE 
          SUBROUTINE SOLVE (A, B, N) 
             INTEGER, INTENT (IN)          :: N 
             REAL, INTENT(OUT)             :: A 
             REAL, INTENT(IN), OPTIONAL    :: B 
          END SUBROUTINE SOLVE 
       END INTERFACE 
 
       REAL X 
!      Note that A, B and N are not specified as REAL 
!      or INTEGER in this unit. 
 
       CALL SOLVE(N=100,A=X) 
 
       END 
 
       SUBROUTINE SOLVE(A, B, N) 
       REAL, OPTIONAL, INTENT (IN)  :: B 
       IF (PRESENT(B)) THEN 
              TEMP_B = B 
       ELSE 
              TEMP_B = 20.0 
       END IF 
       A = TEMP_B + N 
       RETURN 
       END 
