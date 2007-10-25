! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1511.f90


PROGRAM ch1511
IMPLICIT NONE
REAL :: Result,N,R
  PRINT *,' Type in N and R'
  READ *,N,R
! NUMBER OF POSSIBLE COMBINATIONS THAT CAN 
! BE FORMED WHEN
! R OBJECTS ARE SELECTED OUT OF A GROUP OF N
! N!/R!(N-R)!

  !Reaching Constants: Stirling(*) = TOP, N=TOP, R=TOP, Result = TOP
  Result=Stirling(N)/(Stirling(R)*Stirling(N-R))

  !Reaching Constants: Results = TOP
  PRINT *,Result

  !Reaching Constants: N = R = TOP
  PRINT *,N,R
CONTAINS
REAL FUNCTION Stirling (X)
  REAL , INTENT(IN) :: X
  REAL , PARAMETER :: PI=3.1415927, E =2.7182828


  !Reaching Constants: PI = 3.1415927, E = 2.7182828, X = TOP, Stirling = TOP
  Stirling=SQRT(2.*PI*X) * (X/E)**X
END FUNCTION Stirling
END PROGRAM ch1511 
