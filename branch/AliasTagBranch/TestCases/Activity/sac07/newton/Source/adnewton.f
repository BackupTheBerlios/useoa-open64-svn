      PROGRAM ADNEWTON
C     .. Parameters ..
      INTEGER PMAX
      PARAMETER (PMAX=2)
C     .. Local Scalars ..
      DOUBLE PRECISION DUMMY,TEMP,TOL
      INTEGER INFO
C     .. Local Arrays ..
      DOUBLE PRECISION G_X(PMAX,2),G_Y(PMAX,2),X(2),Y(2)
      INTEGER IPIV(2)
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
   10 IF (DLANGE('1',2,1,Y,2,DUMMY).LT.TOL) GO TO 20
c
c	compute function and Jacobian at current iterate
c
      G_X(1,1) = 1.0
      G_X(1,2) = 0.0
      G_X(2,1) = 0.0
      G_X(2,2) = 1.0
      CALL G_FUNC(2,X,G_X,PMAX,Y,G_Y,PMAX)
c
c	transpose g_y
c
      TEMP = G_Y(2,1)
      G_Y(2,1) = G_Y(1,2)
      G_Y(1,2) = TEMP
c
c	solve J * s = - f and update x = x + s
c
      Y(1) = -Y(1)
      Y(2) = -Y(2)
      CALL DGESV(2,1,G_Y,PMAX,IPIV,Y,2,INFO)
      X(1) = X(1) + Y(1)
      X(2) = X(2) + Y(2)
c
c	compute new function value
c
      CALL FUNC(X,Y)
      WRITE (*,FMT=1000) 'Current Function Value:',Y(1),Y(2)
      GO TO 10
   20 CONTINUE
      WRITE (*,FMT=1000) 'Root is approximately:',X(1),X(2)
 1000 FORMAT (a,1x,2 (d15.8,2x))
      END
