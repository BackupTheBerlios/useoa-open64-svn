      PROGRAM NEWTON

C     .. Local Scalars ..
      DOUBLE PRECISION DUMMY,TOL
      INTEGER INFO
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION X(2),Y(2),YPRIME(2,2)
      INTEGER IPIV(2)
C     ..
C     .. External Subroutines ..
      EXTERNAL DGESV,FPRIME,FUNC
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DLANGE
      EXTERNAL DLANGE

C     ..
      TOL = 1.0E-12

      WRITE (*,FMT=*) 'Input 2-element starting vector '
      READ (*,FMT=*) X(1),X(2)
      WRITE (*,FMT=*) 'Starting value is (',X(1),',',X(2),')'

      CALL FUNC(X,Y)
c
c	check for convergence
c      (very simplistic, based only on norm of Y)
c
   10 IF (DLANGE('1',2,1,Y,2,DUMMY).LT.TOL) GO TO 20
c
c	compute function and Jacobian at current iterate
c
      CALL FPRIME(X,Y,YPRIME)
c
c	solve J * s = - f
c      and update x = x + s
c
      Y(1) = -Y(1)
      Y(2) = -Y(2)
      CALL DGESV(2,1,YPRIME,2,IPIV,Y,2,INFO)
      X(1) = X(1) + Y(1)
      X(2) = X(2) + Y(2)
c
c	compute new function value
c
      CALL FUNC(X,Y)
      WRITE (*,FMT=1000) 'Current Function Value:',Y(1),Y(2)
      GO TO 10

   20 CONTINUE
      WRITE (*,FMT=1000) 'Minimum is approximately:',X(1),X(2)
 1000 FORMAT (a,1x,2 (d15.8,2x))
      END
