
      
       IMPLICIT NONE 
       INTERFACE 
          SUBROUTINE SOLVE (A, B, N) 
             INTEGER, INTENT (IN)          :: N 
             REAL, INTENT(OUT)             :: A 
             REAL, INTENT(IN), OPTIONAL    :: B 
          END SUBROUTINE SOLVE 
       END INTERFACE 
 
       REAL X 
 
       CALL SOLVE(N=100,A=X)           
c                                Formal => Actual       
c                                *A     => MemRefNode(X)
c                                *N     => MemRefNode(UnnamedRef(100))
 
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
