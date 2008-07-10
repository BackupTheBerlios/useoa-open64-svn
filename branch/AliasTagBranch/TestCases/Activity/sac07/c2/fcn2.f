C     File:  FCN2.f
      SUBROUTINE FCN2(M,X,Y,YP)
      INTEGER  N
      DOUBLE PRECISION X, Y(M), YP(M)
      INTEGER        ID, IWT
      DOUBLE PRECISION W(20)
      COMMON         /STCOM5/W, IWT, N, ID
 
C Don't use weights
      iwt = -1

      id = 3
      n = m
 
      CALL FCN(X,Y,YP)
      RETURN
      END
