
! Example of Keyword Optional Arguments
    
! Keyword Technique must be used to omit an optional argument that 
! is not at the end of the list. For this reason, optional arguments 
! require explicit procedure interfaces.

! During execution of a procedure with an optional dummy argument, 
! it is usually necessary to know in that particular reference if
! an actual argument has been supplied for that dummy argument.
! The PRESENT intrinsic function is available for that purpose.
      
! ============================================================
! ParamBindings Output:
!       
      
      
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
       
