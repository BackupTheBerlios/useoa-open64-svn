MODULE Precisions
INTEGER,PARAMETER:: Long=SELECTED_REAL_KIND(15,307)
END MODULE Precisions

SUBROUTINE Runge_Kutta_Merson(Y,FUN,IFAIL,N,A,B,Tol)
!
! Runge-Kutta-Merson method for the solution
! of a system of N
! 1st order initial value ordinary
! differential equations.
! The routine tries to integrate from T=A to T=B with
! initial conditions in Y, subject to the
! condition that the
! Absolute Error Estimate <= Tol. The step length is
! adjusted automatically to meet this condition.
! If the routine is successful it returns with
! IFAIL = 0, T=B and
! the solution in Y.
! 
USE Precisions
!
IMPLICIT NONE
! Define arguments
!
REAL (Long),INTENT(INOUT):: Y(:)
REAL(Long), INTENT(IN)::A,B,Tol
INTEGER,INTENT(IN)::N
INTEGER,INTENT(OUT)::IFAIL
!
INTERFACE
  SUBROUTINE FUN(T,Y,F,N)
  USE Precisions
  IMPLICIT NONE
  REAL(Long),INTENT(IN),DIMENSION(:)::Y
  REAL(Long),INTENT(OUT),DIMENSION(:)::F
  REAL(Long),INTENT(IN)::T
  INTEGER,INTENT(IN)::N
  END SUBROUTINE FUN
END INTERFACE
!
! Local variables
!
REAL(Long), DIMENSION(1:SIZE(Y)):: &
        S1,S2,S3,S4,S5,New_Y_1,New_Y_2,Error
REAL(Long)::T,H,H2,H3,H6,H8,Factor=1.E-2_Long
REAL(Long)::Smallest_step=1.E-6_Long,Max_Error
INTEGER::No_of_steps=0
!
  IFAIL=0
!
! Check input parameters
!
  IF(N <= 0 .OR. A == B .OR. Tol <= 0.0) THEN
    IFAIL = 1
    RETURN
  ENDIF
!
! Initialize T to be start of interval and
! H to be 1/100 of interval
  T=A
  H=(B-A)/100.0_Long
  DO                ! Beginning of Repeat loop
    H2=H/2.0_Long
    H3=H/3.0_Long
    H6=H/6.0_Long
    H8=H/8.0_Long
!
! Calculate S1,S2,S3,S4,S5
!
! S1=F(T,Y)

    CALL FUN(T,Y,S1,N)

    New_Y_1=Y+H3*S1

! S2 = F(T+H/3,Y+H/3*S1)

    CALL FUN(T+H3,New_Y_1,S2,N)
    New_Y_1=Y+H6*S1+H6*S2

! S3=F(T+H/3,Y+H/6*S1+H/6*S2)

    CALL FUN(T+H3,New_Y_1,S3,N)

    New_Y_1=Y+H8*(S2+3.0_Long*S3)

! S4=F(T+H/2,Y+H/8*(S2+3*S3))

    CALL FUN(T+H2,New_Y_1,S4,N)
    New_Y_1=Y+H2*(S1-3.0_Long*S3+4.0_Long*S4)

! S5=F(T+H,Y+H/2*(S1-3*S3+4*S4))

    CALL FUN(T+H,New_Y_1,S5,N)
!
! Calculate values at T+H
!
    New_Y_1=Y+H6*(S1+4.0_Long*S4+S5)
    New_Y_2=Y+H2*(S1-3.0_Long*S3+4.0*S4)
!
! Calculate error estimate
!
    Error=ABS(0.2_Long*(New_Y_1-New_Y_2))
    Max_Error=MAXVAL(Error)
    IF(Max_Error > Tol) THEN
!
! Halve step length and try again
!
      IF(ABS(H2) < Smallest_step) THEN
        IFAIL = 2
        RETURN
      ENDIF
      H=H2
    ELSE
!
! Accepted approximation so overwrite Y with Y_new_1,
! and T with T+H
!
      Y=New_Y_1
      T=T+H
!
! Can next step be doubled?
!
      IF(Max_Error*Factor < Tol)THEN
        H=H*2.0_Long
      ENDIF
!
! Does next step go beyond interval end B,
! if so set H = B-T
!
      IF(T+H > B) THEN
        H=B-T
      ENDIF
      No_of_steps=No_of_steps+1
    ENDIF
    IF(T >= B) EXIT         ! End of repeat loop
  END DO
END SUBROUTINE Runge_Kutta_Merson
PROGRAM ch2602
USE Precisions
IMPLICIT NONE
REAL(Long),Dimension(:),Allocatable::Y
REAL(Long)::A,B,Tol 
INTEGER::N,IFAIL,All_stat
INTERFACE
  SUBROUTINE Runge_Kutta_Merson(Y,FUN,IFAIL,N,A,B,Tol)
    USE Precisions
    IMPLICIT NONE
    REAL(Long),INTENT(INOUT) :: Y(:)
    REAL(Long),INTENT(IN)::A,B,Tol
    INTEGER,INTENT(IN)::N
    INTEGER,INTENT(OUT)::IFAIL
    INTERFACE
      SUBROUTINE FUN(T,Y,F,N)
        USE Precisions   
        IMPLICIT NONE
        REAL(Long), INTENT(IN),DIMENSION(:)::Y
        REAL(Long), INTENT(OUT),DIMENSION(:)::F
        REAL(Long), INTENT(IN)::T
        INTEGER,INTENT(IN)::N
      END SUBROUTINE FUN
    END INTERFACE
  END SUBROUTINE Runge_Kutta_Merson
END INTERFACE
!
INTERFACE
  SUBROUTINE Fun1(T,Y,F,N)
  USE Precisions
  IMPLICIT NONE
  REAL(Long), INTENT(IN),DIMENSION(:)::Y
  REAL(Long), INTENT(OUT),DIMENSION(:)::F
  REAL(Long),INTENT(IN):: T
  INTEGER, INTENT(IN):: N
  END SUBROUTINE Fun1
END INTERFACE
!
  PRINT *,'Input no of equations'
  READ*,N
!
! Allocate space for Y - checking to see that it
! allocates properly
!
  ALLOCATE(Y(1:N),STAT=All_stat)
  IF(All_stat /= 0) THEN
    PRINT * , ' Not enough memory'
    PRINT * , ' array Y is not allocated'
    STOP
  ENDIF
  PRINT *,' Input start and end of interval over'
  PRINT *,' which equations to be solved'
  READ *,A,B
  PRINT *,"Input ic's"
  READ *,Y(1:N)
  PRINT *,'Input Tolerance'
  READ *,Tol
  PRINT *,'At T= ',A
  PRINT *,'Initial conditions are :',Y(1:N)
  CALL Runge_Kutta_Merson(Y,Fun1,IFAIL,N,A,B,Tol)
  IF(IFAIL /= 0) THEN
    PRINT *,'Integration stopped with IFAIL = ',IFAIL
  ELSE
    PRINT *,'at T= ',B
    PRINT*,'Solution is: ',Y(1:N)
  ENDIF
END PROGRAM ch2602
Subroutine Fun1(T,Y,F,N)
  USE Precisions
  IMPLICIT NONE
  REAL(Long),INTENT(IN),DIMENSION(:)::Y
  REAL(Long),INTENT(OUT),DIMENSION(:)::F
  REAL(Long),INTENT(IN)::T
  INTEGER,INTENT(IN)::N
!
  F(1)=TAN(Y(3))
  F(2)=-0.032_Long*F(1)/Y(2)-0.02_Long*Y(2)/COS(Y(3))
  F(3)=-0.032_Long/(Y(2)*Y(2))
END SUBROUTINE Fun1


