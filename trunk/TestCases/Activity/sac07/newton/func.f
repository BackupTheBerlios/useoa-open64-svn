      SUBROUTINE FUNC(X,Y)
      DOUBLE PRECISION X(2),Y(2)

c$openad INDEPENDENT(x)
      Y(1) = 10.0* (X(2)-X(1)*X(1))
      Y(2) = 1.0 - X(1)
c$openad DEPENDENT(y)
      RETURN
      END
