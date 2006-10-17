      subroutine head(x,y)
      double precision x(4),y(4)
      call foo(x(1),x(2),y(1))
      y(2)=sin(x(1)*x(2))
      y(3)=sin(x(3))
      y(4)=cos(x(4))
      end subroutine head

      subroutine foo(a,b,c) 
      double precision a,b,c

      call bar(b)
      c=a*a+b

      end subroutine

      subroutine bar(a)
      double precision a

      a=cos(a*a)

      end subroutine
